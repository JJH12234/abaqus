# -*- coding: utf-8 -*-
"""
Parametric modeling for Abaqus/CAE with assembly- and part-level meshing,
preserving original Dependent/Independent instance status.

- Keeps original Dependent/Independent flags (no auto-conversion).
- Mesh seeding supports:
    * Part-level (seedPart / seedEdgeBySize / seedEdgeByNumber) for Dependent flow
    * Assembly-level on instances when scope=='rootAssembly' or when the part has Independent instances
- Regeneration covers all parts and also the rootAssembly.

Python: Abaqus/CAE (2.7)
"""

import xlrd
from abaqus import *              # mdb, session, etc.
from abaqusConstants import *     # ON/OFF, FINER, etc.

VERBOSE = 0  # 0 minimal; 1 normal; 2 include tracebacks


def _print_traceback():
    if VERBOSE >= 2:
        try:
            import traceback
            traceback.print_exc()
        except Exception:
            pass


# -----------------------------
# Helpers
# -----------------------------

def get_current_model():
    """Return the model currently displayed in the active viewport.
    If it's Model-0, prompt to copy or continue."""
    viewport = session.currentViewportName
    modelname = session.sessionState[viewport]['modelName']
    flag = None
    if 'Model-0' in modelname:
        flag = getWarningReply(
            'WARNING: Edit Model-0 is not recommended for user!\n'
            ' YES-continue; No-copyNew;', (YES, NO, CANCEL))
    if flag == NO:
        newname = modelname.replace('Model-0', 'NewModel')
        if newname in mdb.models:
            newname += '-Copy'
        mdb.Model(name=newname, objectToCopy=mdb.models[modelname])
        new_model = mdb.models[newname]
        session.viewports[viewport].setValues(displayedObject=new_model.rootAssembly)
        return new_model
    elif flag == CANCEL:
        raise Exception('User cancelled editing Model-0')
    return mdb.models[modelname]


def clear_assembly_orphan_mesh(m):
    """Delete meshes from all instances in the root assembly (cleanup)."""
    a = m.rootAssembly
    for _, inst in a.instances.items():
        try:
            a.deleteMesh(regions=(inst,))
        except Exception:
            pass


def _has_independent_instance(m, part_name):
    """Return True if the given part has at least one Independent instance in the assembly."""
    a = m.rootAssembly
    for inst in a.instances.values():
        if getattr(inst, 'partName', None) == part_name:
            if getattr(inst, 'dependent', None) == OFF:  # OFF == Independent
                return True
    return False


# -----------------------------
# Core driver
# -----------------------------

def pre_paraModeling(ParaList):
    """Main driver for parametric modeling + meshing with clean logging."""
    m = get_current_model()

    try:
        m.rootAssembly.regenerate()
    except Exception as e:
        print("[WARN] Assembly regen failed (non-fatal, will proceed): %s" % e)

    # 拆分参数
    sketch_data, features_data, mesh_data = process_parameters(ParaList)

    # ---- Sketch 参数按 (part, feature) 分组并应用 ----
    p_f_s = {}
    for row in sketch_data:
        if not row or len(row) < 5:
            continue
        key = (row[3], row[4])
        p_f_s.setdefault(key, []).append(row)

    for (partname, featurename), rows in p_f_s.items():
        partname_str = str(partname).strip()
        featname_str = str(featurename).strip()
        if partname_str not in m.parts:
            print(u"[WARN] Part '{}' not found in model.".format(partname_str))
            continue
        part = m.parts[partname_str]
        # feature resolve (strict, then case-insensitive)
        if featname_str in part.features:
            feat = part.features[featname_str]
        else:
            hit = None
            for k in part.features:
                if k.strip().lower() == featname_str.lower():
                    hit = k
                    break
            if hit is None:
                print(u"{p} has no {f} feature. Available: {keys}".format(
                    p=partname_str, f=featname_str,
                    keys=u', '.join(part.features.keys())))
                continue
            feat = part.features[hit]
        try:
            paraModeling_sketch(m, feat, rows)
        except Exception:
            print("[WARN] Failed to update sketch parameters on %s.%s" %
                  (partname_str, featname_str))
            _print_traceback()

    # ---- Feature 标量参数 ----
    for row in features_data:
        if not row or len(row) < 5:
            continue
        part_key = str(row[3]).strip()
        feat_key = str(row[4]).strip()
        if part_key not in m.parts:
            print(u"[WARN] Part '{}' not found in model.".format(part_key))
            continue
        part = m.parts[part_key]
        try:
            if feat_key not in part.features:
                hit = None
                for k in part.features:
                    if k.strip().lower() == feat_key.lower():
                        hit = k
                        break
                if hit is None:
                    print(u"{p} has no {f} feature. Available: {keys}".format(
                        p=part_key, f=feat_key,
                        keys=u', '.join(part.features.keys())))
                    continue
                f = part.features[hit]
            else:
                f = part.features[feat_key]
            paraModeling_features(f, row)
        except KeyError:
            print(u"{p} has no {f} feature".format(p=part_key, f=feat_key))
        except Exception as e:
            print("[WARN] Set feature value failed on %s.%s: %s" % (part_key, feat_key, e))
            _print_traceback()

    # ---- Regen：零件 + 装配 ----
    paraModeling_regen(m, regen_assembly=True)

    # ---- 网格流程 ----
    clear_assembly_orphan_mesh(m)
    asm_touched = apply_mesh_instructions(m, mesh_data)
    mesh_regen(m, delete_first=True, assembly_instances=asm_touched)

    # ---- 最终装配 regen（单一可信结果）----
    ass_regen(m)


def paraModeling_sketch(m, feature, paralist):
    """Modify a sketch's parameters from given rows."""
    s0 = feature.sketch
    m.ConstrainedSketch(name='__edit__', objectToCopy=s0)
    s1 = m.sketches['__edit__']
    s1.setPrimaryObject(option=SUPERIMPOSE)
    changed = []
    for row in paralist:
        paraname = str(row[0]).strip()
        expression = str(row[1]).strip()
        if paraname in s1.parameters:
            s1.parameters[paraname].setValues(expression=expression)
        else:
            keys = list(s1.parameters.keys())
            if keys:
                s1.Parameter(name=paraname, expression=expression,
                             previousParameter=keys[-1])
            else:
                s1.Parameter(name=paraname, expression=expression)
        changed.append((paraname, expression))
    try:
        s1.unsetPrimaryObject()
        feature.setValues(sketch=s1)
    except Exception as e:
        print("[ERROR] apply sketch to feature '%s' failed: %s" % (feature.name, e))
        for pn, ex in changed:
            print("  %s = %s" % (pn, ex))
        try:
            del m.sketches['__edit__']
        except Exception:
            pass
        _print_traceback()
        raise
    try:
        del m.sketches['__edit__']
    except Exception:
        pass


def paraModeling_features(feature, data):
    """Set feature scalar value from data[1]."""
    feature.setValues(data[1])


def process_parameters(data):
    """Classify rows into sketch/feature/mesh groups."""
    sketch_data, features_data, mesh_data = [], [], []
    sorted_data = sorted(data, key=lambda x: x[3])
    for row in sorted_data:
        if not row or len(row) < 5:
            continue
        typ = str(row[2]).strip()
        if typ.split('.')[0] == 'sketch':
            sketch_data.append(row)
        elif typ in ('seedPart', 'seedEdgeBySize', 'seedEdgeByNumber', 'seedPartInstance'):
            mesh_data.append(row)
        else:
            features_data.append(row)
    return sketch_data, features_data, mesh_data


# -----------------------------
# Regeneration / Meshing
# -----------------------------

def paraModeling_regen(m, regen_assembly=True):
    """Regenerate all parts and (optionally) the root assembly."""
    # parts
    for partname, part in m.parts.items():
        try:
            part.regenerate()
        except Exception as e:
            print("%s regen fails! %s" % (partname, e))
            _print_traceback()
        # Report suppressed features to help locate red X
        try:
            suppressed_feats = [fname for fname, feat in part.features.items()
                                if getattr(feat, 'suppressed', False)]
            if suppressed_feats:
                print("[INFO] Part '%s' has suppressed features: %s" %
                      (partname, ", ".join(suppressed_feats)))
        except Exception:
            pass
    # assembly
    if regen_assembly:
        try:
            m.rootAssembly.regenerate()
        except Exception as e:
            print("[WARN] rootAssembly.regenerate() failed (non-fatal): %s" % e)
            _print_traceback()


def _resolve_assembly_edges(a, inst_name, set_name):
    """Return an EdgeArray for assembly seeding.
    Priority: assembly.sets -> instance.sets -> all edges of the instance."""
    try:
        if set_name and set_name in a.sets:
            edges = a.sets[set_name].edges
            if len(edges):
                return edges
    except Exception:
        pass
    try:
        inst = a.instances[inst_name]
        if (set_name and hasattr(inst, 'sets') and set_name in inst.sets
                and len(inst.sets[set_name].edges)):
            return inst.sets[set_name].edges
        return inst.edges  # fallback: all edges on this instance
    except Exception:
        return None


def apply_mesh_instructions(m, mesh_rows):
    """Apply mesh seeds for Part-level and Assembly-level (rootAssembly) rows.

    Returns:
        set of instance names that were touched at assembly level (for later generateMesh).
    """
    if not mesh_rows:
        return set()

    a = m.rootAssembly
    assembly_instances_touched = set()

    for row in mesh_rows:
        try:
            set_name  = str(row[0]).strip()     # 集合名（可空）
            val_raw   = row[1]                  # 数值
            method    = str(row[2]).strip()     # 方法
            scope     = str(row[3]).strip()     # 'rootAssembly' 或 Part 名
            extra     = str(row[4]).strip() if len(row) > 4 else ''  # 实例名 (装配级)

            # —— 统一数值类型 —— #
            if method == 'seedEdgeByNumber':
                try:
                    val_int = int(float(val_raw))
                except Exception:
                    print(u"[Mesh] Bad integer in column-2 for row: {}".format(row))
                    continue
            else:
                try:
                    val = float(val_raw)
                except Exception:
                    print(u"[Mesh] Bad float in column-2 for row: {}".format(row))
                    continue

            # ===== 装配级：对实例操作 =====
            if scope.lower() == 'rootassembly':
                inst_name = extra
                if inst_name not in a.instances:
                    print(u"[Mesh] Instance '{}' not found in rootAssembly.".format(inst_name))
                    continue

                # 删除该实例的旧网格（可选）
                try:
                    a.deleteMesh(regions=(a.instances[inst_name],))
                except Exception:
                    pass

                # 1) 全局种子到实例
                if method == 'seedPartInstance':
                    try:
                        a.seedPartInstance(regions=(a.instances[inst_name],),
                                           size=val, deviationFactor=0.1, minSizeFactor=0.1)
                    except Exception as e:
                        print(u"[Mesh] seedPartInstance on '{}' failed: {}".format(inst_name, e))
                        _print_traceback()
                        continue
                    assembly_instances_touched.add(inst_name)
                    continue

                # 2) 边种子（优先集合，否则全边）
                picked = _resolve_assembly_edges(a, inst_name, set_name)
                if picked is None or len(picked) == 0:
                    print(u"[Mesh] No edges resolved for instance '{}' (set='{}'); skip.".format(
                        inst_name, set_name))
                    continue

                try:
                    if method == 'seedEdgeBySize':
                        a.seedEdgeBySize(edges=picked, size=val,
                                         deviationFactor=0.1, minSizeFactor=0.1,
                                         constraint=FINER)
                    elif method == 'seedEdgeByNumber':
                        a.seedEdgeByNumber(edges=picked, number=val_int, constraint=FINER)
                    else:
                        print(u"[Mesh] Unknown assembly method '{}'; skip.".format(method))
                        continue
                except Exception as e:
                    print(u"[Mesh] Assembly seeding failed on '{}': {}".format(inst_name, e))
                    _print_traceback()
                    continue

                assembly_instances_touched.add(inst_name)
                continue  # next row

            # ===== 零件级：如果该 Part 存在 Independent 实例，则自动上浮到装配级 =====
            part_name = scope
            if part_name not in m.parts:
                print(u"[Mesh] Part '{}' not found; skip.".format(part_name))
                continue

            if _has_independent_instance(m, part_name):
                # 对所有 Independent 实例施加同样的种子
                for nm, inst in a.instances.items():
                    if inst.partName == part_name and getattr(inst, 'dependent', None) == OFF:
                        try:
                            a.deleteMesh(regions=(inst,))
                        except Exception:
                            pass
                        if method == 'seedPart':
                            a.seedPartInstance(regions=(inst,),
                                               size=float(val_raw), deviationFactor=0.1, minSizeFactor=0.1)
                        elif method in ('seedEdgeBySize', 'seedEdgeByNumber'):
                            picked = _resolve_assembly_edges(a, nm, set_name)
                            if picked is None or len(picked) == 0:
                                print(u"[Mesh] No edges for instance '{}' (set='{}').".format(nm, set_name))
                                continue
                            if method == 'seedEdgeBySize':
                                a.seedEdgeBySize(edges=picked, size=float(val_raw),
                                                 deviationFactor=0.1, minSizeFactor=0.1, constraint=FINER)
                            else:
                                a.seedEdgeByNumber(edges=picked, number=int(float(val_raw)), constraint=FINER)
                        else:
                            print(u"[Mesh] Unknown method '{}' for Independent flow; skip.".format(method))
                            continue
                        assembly_instances_touched.add(nm)
                # 不再在 Part 级处理该行
                continue

            # ===== 零件级（仅 Dependent 流程） =====
            p = m.parts[part_name]
            if method == 'seedPart':
                try:
                    p.deleteMesh()
                except Exception:
                    pass
                p.seedPart(size=val, deviationFactor=0.1, minSizeFactor=0.1)

            elif method == 'seedEdgeBySize':
                if set_name in p.sets:
                    edges = p.sets[set_name].edges
                    if len(edges) == 0:
                        print(u"[Mesh] Set '{}' in part '{}' has no edges.".format(set_name, part_name))
                        continue
                    p.seedEdgeBySize(edges=edges, size=val,
                                     deviationFactor=0.1, minSizeFactor=0.1,
                                     constraint=FINER)
                else:
                    print(u"[Mesh] Set '{}' not found in part '{}'.".format(set_name, part_name))

            elif method == 'seedEdgeByNumber':
                if set_name in p.sets:
                    edges = p.sets[set_name].edges
                    if len(edges) == 0:
                        print(u"[Mesh] Set '{}' in part '{}' has no edges.".format(set_name, part_name))
                        continue
                    p.seedEdgeByNumber(edges=edges, number=val_int, constraint=FINER)
                else:
                    print(u"[Mesh] Set '{}' not found in part '{}'.".format(set_name, part_name))
            else:
                print(u"[Mesh] Unknown method '{}'; skip.".format(method))

        except Exception as e:
            print(u"[Mesh] Fail on row {} -> {}".format(row, str(e)))
            _print_traceback()

    return assembly_instances_touched


def mesh_regen(m, delete_first=True, assembly_instances=None):
    """Regenerate meshes on parts and (if needed) on assembly instances."""
    try:
        parts = getattr(m, 'parts', {})
    except Exception as e:
        print("mesh_regen: cannot access model.parts: %s" % e)
        return [], []

    a = m.rootAssembly
    ok, fail = [], []

    # ① Part 级：仅对“没有 Independent 实例”的 Part 生成网格
    for partname, p in parts.items():
        if _has_independent_instance(m, partname):
            continue  # 交给装配级
        try:
            if delete_first:
                try:
                    p.deleteMesh((p,))
                except TypeError:
                    p.deleteMesh()
                except Exception as de:
                    if VERBOSE >= 1:
                        print("[WARN] deleteMesh on %s: %s" % (partname, de))
            p.generateMesh()
            print("%s regen OK" % partname)
            ok.append(partname)
        except Exception as e:
            print("%s regen fails! %s" % (partname, e))
            fail.append(partname)
            _print_traceback()

    # ② 装配级：对所有 Independent 实例（以及被本轮种子的实例）生成网格
    try:
        inst_list = []
        touched = set(assembly_instances or [])
        for nm, inst in a.instances.items():
            if getattr(inst, 'dependent', None) == OFF:  # Independent
                inst_list.append(inst)
                touched.add(nm)
        if inst_list:
            try:
                a.generateMesh(regions=tuple(inst_list))
                print("[ASM] generateMesh on instances: %s" % ", ".join(sorted(touched)))
            except TypeError:
                a.generateMesh()
                print("[ASM] generateMesh on rootAssembly (fallback)")
    except Exception as e:
        print("[WARN] rootAssembly.generateMesh failed: %s" % e)
        _print_traceback()

    print("mesh_regen summary: %d ok, %d fail" % (len(ok), len(fail)))
    if fail:
        print("failed parts: %s" % ", ".join(fail))
    return ok, fail


def ass_regen(m):
    """Final assembly regeneration only; if it fails, report now."""
    try:
        m.rootAssembly.regenerate()
    except Exception as e:
        print("Assembly regen fails! %s" % e)
        _print_traceback()


# -----------------------------
# Excel I/O
# -----------------------------

def read_excel_to_tuple(xls_path, xls_sheetname):
    """Read an Excel sheet using xlrd and return a tuple of row tuples."""
    book = xlrd.open_workbook(xls_path)
    sheet = book.sheet_by_name(xls_sheetname)
    return tuple(tuple(sheet.row_values(i)) for i in range(sheet.nrows))


def pre_paraModeling_main(xls_path, xls_sheetname):
    """Read Excel and execute parametric modeling."""
    data = read_excel_to_tuple(xls_path, xls_sheetname)
    pre_paraModeling(data)


if __name__ == '__main__':
    demo = (u'd:/SIMULIA/EstProducts/2023/win_b64/code/python2.7/lib/'
            u'abaqus_plugins/STPM_test1034/ParaModelingData.xls')
    pre_paraModeling_main(demo, 'XGB')
