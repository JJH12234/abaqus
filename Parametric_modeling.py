# -*- coding: utf-8 -*-
"""
Parametric modeling for Abaqus/CAE with enhanced handling of dependent and
independent instances, and cleaner logging (no mid-process assembly traceback).

This script reads parameter definitions from an Excel sheet and applies them
to sketches and features in the current Abaqus model. It also performs
meshing instructions and handles the regeneration of parts and the root
assembly. To avoid confusing "Assembly regen ... FeatureError" messages when
the model is still in a transient state, this version does NOT call
m.rootAssembly.regenerate() inside paraModeling_regen(). Only the final
ass_regen(m) is used to report the end result.

Python: Abaqus/CAE (2.7)
"""

import xlrd
from abaqus import *              # provided by Abaqus (mdb, session, etc.)
from abaqusConstants import *     # constants like ON/OFF, FINER, etc.

# -----------------------------
# Verbosity control
# -----------------------------
VERBOSE = 0  # 0: minimal prints; 1: normal; 2: include Python tracebacks

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


def ensure_instances_dependent(m, part_name):
    """Convert independent instances of part_name to Dependent and clear their meshes."""
    a = m.rootAssembly
    changed = []
    for iname, inst in a.instances.items():
        if getattr(inst, 'partName', None) != part_name:
            continue
        dep_flag = getattr(inst, 'dependent', None)
        if dep_flag is None or dep_flag == OFF:
            try:
                a.deleteMesh(regions=(inst,))
            except Exception:
                pass
            try:
                a.makeDependent(instances=(inst,))
            except Exception as e:
                print("[Mesh] cannot make instance '%s' dependent: %s" % (iname, e))
                continue
            changed.append(iname)
    if changed:
        print("[Mesh] Instances made Dependent for part '%s': %s" %
              (part_name, ", ".join(changed)))


def clear_assembly_orphan_mesh(m):
    """Delete meshes from all instances in the root assembly (cleanup)."""
    a = m.rootAssembly
    for iname, inst in a.instances.items():
        try:
            a.deleteMesh(regions=(inst,))
        except Exception:
            pass

# -----------------------------
# Core driver
# -----------------------------

def pre_paraModeling(ParaList):
    """Main driver for parametric modeling + meshing with clean logging."""
    m = get_current_model()

    # Ensure all instances are Dependent BEFORE any geometry changes
    for part_name in list(m.parts.keys()):
        try:
            ensure_instances_dependent(m, part_name)
        except Exception as e:
            print("[WARN] ensure_instances_dependent on part '%s' failed: %s" % (part_name, e))

    # Assembly may still be transient here; keep this as a soft WARN without traceback
    try:
        m.rootAssembly.regenerate()
    except Exception as e:
        print("[WARN] Assembly regen failed after makeDependent (non-fatal, will proceed): %s" % e)

    # Split parameters: sketch / feature / mesh
    sketch_data, features_data, mesh_data = process_parameters(ParaList)

    # ---- Apply sketch parameters grouped by (part, feature) ----
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
        # modify sketch params
        try:
            paraModeling_sketch(m, feat, rows)
        except Exception:
            print("[WARN] Failed to update sketch parameters on %s.%s" %
                  (partname_str, featname_str))
            _print_traceback()

    # ---- Apply feature parameters ----
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

    # ---- Regenerate parts (NO assembly regen here) ----
    paraModeling_regen(m)

    # ---- Mesh workflow ----
    clear_assembly_orphan_mesh(m)
    apply_mesh_instructions(m, mesh_data)
    mesh_regen(m)

    # ---- Final assembly regen (single source of truth) ----
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
        elif typ in ('seedPart', 'seedEdgeBySize', 'seedEdgeByNumber'):
            mesh_data.append(row)
        else:
            features_data.append(row)
    return sketch_data, features_data, mesh_data

# -----------------------------
# Regeneration / Meshing
# -----------------------------

def paraModeling_regen(m):
    """Regenerate all parts; assembly regen is handled by caller later."""
    for partname, part in m.parts.items():
        try:
            part.regenerate()
        except Exception as e:
            print("%s regen fails! %s" % (partname, e))
            _print_traceback()
        # Report suppressed features to help locate red X in Model Tree
        try:
            suppressed_feats = [fname for fname, feat in part.features.items()
                                if getattr(feat, 'suppressed', False)]
            if suppressed_feats:
                print("[INFO] Part '%s' has suppressed features: %s" %
                      (partname, ", ".join(suppressed_feats)))
        except Exception:
            pass
    # IMPORTANT: do NOT call m.rootAssembly.regenerate() here.


def apply_mesh_instructions(m, mesh_rows):
    """Apply mesh seeds; ensure Dependent per part first."""
    if not mesh_rows:
        return
    checked = set()
    for row in mesh_rows:
        try:
            set_name  = str(row[0]).strip()
            val_raw   = row[1]
            method    = str(row[2]).strip()
            part_name = str(row[3]).strip()
            if part_name not in m.parts:
                print(u"[Mesh] Part '{}' not found; skip.".format(part_name))
                continue
            if part_name not in checked:
                ensure_instances_dependent(m, part_name)
                checked.add(part_name)
            p = m.parts[part_name]
            try:
                if method == 'seedEdgeByNumber':
                    val = int(float(val_raw))
                else:
                    val = float(val_raw)
            except Exception:
                print(u"[Mesh] Bad value in column-2 for row: {}".format(row))
                continue
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
                    p.seedEdgeByNumber(edges=edges, number=val, constraint=FINER)
                else:
                    print(u"[Mesh] Set '{}' not found in part '{}'.".format(set_name, part_name))
            else:
                print(u"[Mesh] Unknown method '{}'; skip.".format(method))
        except Exception as e:
            print(u"[Mesh] Fail on row {} -> {}".format(row, str(e)))
            _print_traceback()


def mesh_regen(m, delete_first=True):
    """Delete & regenerate meshes on all parts (no assembly regen here)."""
    try:
        parts = getattr(m, 'parts', {})
    except Exception as e:
        print("mesh_regen: cannot access model.parts: %s" % e)
        return [], []
    ok, fail = [], []
    for partname, p in parts.items():
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
    print("mesh_regen summary: %d ok, %d fail" % (len(ok), len(fail)))
    if fail:
        print("failed parts: %s" % ", ".join(fail))
    return ok, fail


def ass_regen(m):
    """Final assembly regeneration only; if it fails, report now."""
    try:
        m.rootAssembly.regenerate()
        # You can keep a success message here if you want:
        # print("Assembly regen OK")
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
