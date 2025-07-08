# -*- coding: utf-8 -*-
"""
Created on Tue Mar  4 16:58:27 2025

@author: mrvoid
"""
from abaqus import *
from abaqusConstants import *
from collections import OrderedDict
import os
import re
import inspect
import json
filename = inspect.getframeinfo(inspect.currentframe()).filename
script_dir     = os.path.dirname(os.path.abspath(filename))
# ---------- 缩进常量 ----------
_IND1      = '      '     # 一级：6 空格
_IND_STEP  = '   '        # if 嵌套每多一级：+3 空格
_IND_CONT  = '     '      # 行首续行符 & 使用 5 空格

# ---------- 正则 ----------
_RE_COMMENT   = re.compile(r'^[cC\*!]')                       # 列 1 注释
_RE_INC_MIS   = re.compile(r'^[cC]\s*INCLUDE\s+\'ABA', re.I)
_RE_PARAM     = re.compile(r'^\s*(temp\d+|x_[A-Za-z]\w*)\s*=', re.I)
_RE_IFLINE    = re.compile(r'\b(if\b.*then|else\b|elseif\b|end\s*if\b)', re.I)

def _fix_block_spacing(txt):
    """
    把一段 UHARD / CREEP Fortran 代码的缩进、注释对齐到示范格式
    """
    out   = []
    depth = 0                     # 当前 IF 嵌套层数

    for raw in txt.splitlines():
        ln  = raw.rstrip()        # 不动结尾续行 &
        lns = ln.lstrip()

        # ---------- 0. 修错 INCLUDE ----------
        if _RE_INC_MIS.match(ln):
            out.append(_IND1 + "INCLUDE 'ABA_PARAM.INC'")
            continue

        # ---------- 1. 注释行 ----------
        if _RE_COMMENT.match(ln):
            out.append(lns)        # 顶格
            continue

        # ---------- 2. 续行符 & ----------
        if lns.startswith('&'):
            out.append(_IND_CONT + lns)
            continue

        # ---------- 3. IF / ELSE / ELSEIF / END IF ----------
        m_if = _RE_IFLINE.search(ln)
        if m_if:
            indent = _IND1 + _IND_STEP * depth
            kw = m_if.group(1).lower()

            if kw.startswith('end'):          # END IF
                depth = max(depth - 1, 0)
                indent = _IND1 + _IND_STEP * depth
                out.append(indent + lns)
                continue

            if kw.startswith('else'):         # ELSE / ELSEIF
                depth = max(depth - 1, 0)
                indent = _IND1 + _IND_STEP * depth
                out.append(indent + lns)
                if lns.endswith('then'):
                    depth += 1
                continue

            # 普通 IF … THEN
            out.append(indent + lns)
            depth += 1
            continue

        # ---------- 4. “参数常量行” 仅最外层 6 空格 ----------
        if _RE_PARAM.match(ln) and depth == 0:
            out.append(_IND1 + lns)
            continue

        # ---------- 5. 其它可执行语句 ----------
        indent = _IND1 + _IND_STEP * depth
        out.append(indent + lns)

    return '\n'.join(out)
def get_current_model():
    """获取当前视口关联的模型"""
    flag=None
    viewport = session.currentViewportName
    modelname=session.sessionState[viewport]['modelName']
    if 'Model-0' in modelname:
        flag=getWarningReply(
            u'警告： 不推荐用户自行编辑Model-0！\n YES以强制编辑; No建立副本并编辑;'.encode('GB18030'), (YES,NO,CANCEL))
    if flag==NO:
        newname=modelname.replace('Model-0','NewModel')
        if newname in mdb.models.keys():
            newname=newname+'-Copy'
        mdb.Model(name=newname, 
                  objectToCopy=mdb.models[modelname])
        a = mdb.models[newname].rootAssembly
        session.viewports[viewport].setValues(displayedObject=a)
        return mdb.models[newname]
    elif flag==CANCEL:
        raise Exception('User Cancels when edit {mm}'.format(mm=modelname))
        return mdb.models[modelname]
    return mdb.models[modelname]

def pre_materialImport_main(jsondata,aimMaterialName,UVARMnum,SDVnum):
    #jsondata应该是多级字典结构
    m=get_current_model()
    if aimMaterialName in m.materials:
        pass#不修改已有材料其他属性
    else:
        try:
            m.Material(name=aimMaterialName) #新建
        except AbaqusNameError as e:
            raise Exception(u"{}不能作为材料名".format(aimMaterialName).encode('GB18030'))
            return 0
    mm=m.materials[aimMaterialName]
    #filtered_data = {k: v for k, v in jsondata.items() if not k.startswith("user_")} #排除子程序用参数
    filtered_data = {}
    fortran_data = {}
    for k, v in jsondata.items():
        if k.startswith("user_"):
            fortran_data[k] = v
        else:
            filtered_data[k] = v
    data=process_dict(filtered_data) #处理为列表
    for properrow in data:
        addproperty(mm, properrow)
    if UVARMnum>0:
        mm.UserOutputVariables(n=UVARMnum)
    else:
        if getattr(mm,'userOutputVariables',None):
            del mm.userOutputVariables
    if SDVnum>0:
        mm.UserDefinedField()
        mm.Depvar(n=SDVnum)
    else:
        if getattr(mm,'depvar',None):
            del mm.depvar
        if getattr(mm,'userDefinedField',None):
            del mm.userDefinedField
    # print(process_dict(fortran_data))
    Creep, Plastic, user_EquivS,user_Tr,user_Nd, user_CFInter,user_Type = pre_materialImport(jsondata)
    fortran_name=aimMaterialName
    generate_Fortran(Creep,Plastic, user_EquivS,user_Tr,user_Nd, user_CFInter,user_Type,fortran_name)
    return 
# 定义一个递归函数来处理字典
def process_dict(d, path=None):
    if path is None:
        path = []
    result = []
    for key, value in d.items():
        if isinstance(value, dict):
            result.extend(process_dict(value, path + [key]))
        else:
            result.append(path + [key, value])
    return result

def tDepCheck(property_name, property_type, property_data):
    # 定义属性与期望列数的映射
    property_columns = {
        'Density': {'Uniform': 1},
        'Elastic': {'Isotropic': 2},
        'Conductivity': {'Isotropic': 1},
        'Specific Heat': {'*':1},
        'Expansion': {'Isotropic': 1},
        'Plastic': {'Isotropic': 2, 'User': 1},
        'Creep': {
            'Strain-Harding': 3,
            'Time-Harding': 3,
            'Power': 4,
            'Time_Power': 4,
            'User': 0
        },
    }
    # 获取传入数据的列数
    if not property_data:
        return OFF  # 无数据时默认关闭
    # 获取实际数据列数
    num_columns = len(property_data[0])
    
    # 获取期望列数
    columns_spec = property_columns.get(property_name)
    if columns_spec is None:
        return ON  # 属性不在映射中，默认开启
    # 解析期望列数
    if isinstance(columns_spec, dict):
        # 先检查具体类型
        expected_num = columns_spec.get(property_type)
        # 如果具体类型不存在，检查通配符
        if expected_num is None and '*' in columns_spec:
            expected_num = columns_spec['*']
    else:
        # 直接数字类型规格
        expected_num = columns_spec
    
    # 如果未找到具体类型或通配符，默认开启
    if expected_num is None:
        return ON
    
    # 判断列数是否匹配（实际列数 = 期望列数 + 1）
    return ON if num_columns == expected_num + 1 else OFF

def addproperty(mm, datarow):
    property_name = datarow[0]
    property_type = datarow[1]
    table_data = datarow[-1]
    # print(table_data)
    if table_data==((None,),):
        try:
            print(u"{0}数据为空，将尝试删除{0}属性".format(unicode(property_name)).encode('GB18030'))
            try:
                mmp=getattr(mm,property_name[0].lower() + property_name[1:])
                del mmp
            except Exception as e:
                print(u'删除{}失败，请管理员检查关键字'.format(unicode(property_name)).encode('GB18030'))
            return 0
        except Exception as e:
            print(e)
            return 0
    tDCflag = tDepCheck(property_name, property_type, table_data)
    try:
        tryarg=globals()[property_type.upper().replace(' ', '')]
    except (KeyError , AttributeError):
        tryarg=NONE
        if property_name!='Creep': #蠕变属性下'law'参数的常量名比较奇怪
            print(u"{pt}不是abaqusConstants".format(pt=unicode(str(property_type)).encode('GB18030').upper().replace(' ', '')))
            print(u"属性{pn}以默认值建立".format(pn=unicode(property_name)).encode('GB18030'))
    handler_map = {
        # Density处理
        ('Density', 'Uniform'): {
            'method': 'Density',
            'args': {
                'distributionType': UNIFORM,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Density', '*'): {
            'method': 'Density',
            'args': {
                'distributionType': DISCRETE_FIELD,
                'fieldName': property_type,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        # Creep处理
        ('Creep', 'User_defined'): {
            'method': 'Creep',
            'args': {'law': USER, 'table': ()}
        },
        ('Creep', 'Power'): {
            'method': 'Creep',
            'args': {
                'law': POWER_LAW,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Creep', 'Time_Power'): {
            'method': 'Creep',
            'args': {
                'law': TIME_POWER_LAW,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Creep', 'Time-Harding'): {
            'method': 'Creep',
            'args': {
                'law': TIME,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Creep', 'Strain-Harding'): {
            'method': 'Creep',
            'args': {
                'law': STRAIN,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Creep', '*'): {
            'method': 'Creep',
            'args': {
                'law': tryarg,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        # Plastic处理
        ('Plastic','User'):{
            'method': 'Plastic',
            'args': {
                'hardening': USER,
                'table': table_data,
            }
        },
        ('Plastic', '*'): {
            'method': 'Plastic',
            'args': {
                'hardening': tryarg,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        # Specific Heat处理
        ('Specific Heat', '*'): {
            'method': 'SpecificHeat',
            'args': {
                'law': tryarg,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        # 通用类型处理（Elastic, Expansion, Conductivity）
        ('Elastic', '*'): {
            'method': property_name,
            'args': {
                'type':  tryarg,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Expansion', '*'): {
            'method': property_name,
            'args': {
                'type':  tryarg,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Conductivity', '*'): {
            'method': property_name,
            'args': {
                'type':  tryarg,
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('*', '*'): {
            'method': property_name.replace(' ', ''),
            'args': OrderedDict([
                ('type',  tryarg),
                ('table', table_data),
                ('temperatureDependency', tDCflag)
            ])
        }
    }
    
    # 查找处理程序
    handler = None
    specific_key = (property_name, property_type)
    wildcard_key = (property_name, '*')
    any_key = ('*', '*')
    
    if specific_key in handler_map:
        handler = handler_map[specific_key]
    elif wildcard_key in handler_map:
        handler = handler_map[wildcard_key]
    
    # 如果未找到处理程序，抛出异常
    if not handler:
        #raise ValueError(f"No handler for {property_name}/{property_type}")
        print(u"没有名为{property_name}/{property_type}的属性".format(property_name=property_name,property_type=property_type).encode('GB18030'))
        handler = handler_map[any_key]
        method = getattr(mm, handler['method'])
        args_dict = handler['args']
        if tryarg==NONE:
            del method_args['type']
            method(**args_dict)
        else:
            if args_dict:
                # 提取第一个参数的值和剩余参数
                keys = list(args_dict.keys())
                first_value = args_dict[keys[0]]
                remaining_args = {k: args_dict[k] for k in keys[1:]}
                # 调用时第一个参数作为第一个位置的参数传递
                method(first_value, **remaining_args)
        return
    # 获取方法并调用
    method_name = handler['method']
    method_args = handler['args']
    # 检查 mm 对象是否有对应方法
    if not hasattr(mm, method_name):
        #raise AttributeError(f"Method '{method_name}' not found in mm object")
        print(u"方法'{method_name}'在mm对象中没有找到".format(method_name=method_name).encode('GB18030'))
    if tryarg==NONE and hasattr(method_args,'type'):
        del method_args['type']
    method = getattr(mm, method_name)
    method(**method_args)


# 对传入的数据进行预处理并分类
def pre_materialImport(jsondata):
    #jsondata应该是多级字典结构
    filtered_data = {k: v for k, v in jsondata.items()
                     if k.startswith("user_") or k.startswith("Creep") or k.startswith("Plastic")} #仅需要传入子程序的参数
    # 递归处理字典为扁平化列表
    flat_data = process_dict(filtered_data)
    # 初始化分类容器
    Creep = []
    Plastic = []
    user_RepresentativeStress = []
    user_CreepRuptureLife = []
    user_FatigueLife = []
    user_CreepFatigueInteractionCriterion = []
    user_AnalysisType = []
    for i in range(0, len(flat_data)):
        if flat_data[i][0] == "Creep" and flat_data[i][1] == 'User_defined':
            Creep = flat_data[i]
        elif flat_data[i][0] == "Plastic" and flat_data[i][1] == 'User':
            Plastic = flat_data[i]
        elif flat_data[i][0] == "user_RepresentativeStress":
            user_RepresentativeStress = flat_data[i]
        elif flat_data[i][0] == "user_CreepRuptureLife":
            user_CreepRuptureLife = flat_data[i]
        elif flat_data[i][0] == "user_FatigueLife":
            user_FatigueLife = flat_data[i]
        elif flat_data[i][0] == "user_CreepFatigueInteractionCriterion":
            user_CreepFatigueInteractionCriterion = flat_data[i]
        elif flat_data[i][0] == "user_AnalysisType":
            user_AnalysisType = flat_data[i]
    return Creep, Plastic, user_RepresentativeStress,user_CreepRuptureLife,\
           user_FatigueLife,user_CreepFatigueInteractionCriterion,\
           user_AnalysisType
# 定义一个递归函数来处理字典
def process_dict(d, path=None):
    if path is None:
        path = []
    result = []
    for key, value in d.items():
        if isinstance(value, dict):
            result.extend(process_dict(value, path + [key]))
        else:
            result.append(path + [key, value])
    return result

# 生成fortran文件的总体思路
def generate_Fortran(Creep=[],Plastic=[], user_EquivS=[],user_Tr=[],user_Nd=[], user_CFInter=[],user_Type=[],materialName=''):
    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/M225Cr1MoMuBan.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path = os.path.join(script_dir, template_name)
    # 每一个模块接续处理
    if Creep:
        CreepFortran = generate_creep_subroutine(template_path,Creep)
    if user_Tr:
        user_TrFortran = generate_user_CreepRuptureLife(CreepFortran,user_Tr)
    if user_Nd:
        user_NdFortran = generate_user_FatigueLife(user_TrFortran,user_Nd)
    if user_CFInter:
        user_CFInterFortran = generate_user_CreepFatigueInteractionCriterion(user_NdFortran,user_CFInter)
    if user_EquivS:
        user_EquivSFortran = generate_user_RepresentativeStress(user_CFInterFortran,user_EquivS)
    if Plastic:
        user_PlasticFortran = generate_user_Plastic(user_EquivSFortran, Plastic)
    else:
        user_PlasticFortran = ''
    # 写出最终 For 文件
    fortran_file_path='{}\\{}.for'.format(script_dir,materialName)
    with open(fortran_file_path, 'w') as f:
        f.write(user_PlasticFortran)  # 将生成的 Fortran 代码写入文件
    print(u'{}.for 已输出到 {}'.format(materialName,script_dir).encode('GB18030'))
    return fortran_file_path


# 蠕变本构参数传入
def generate_creep_subroutine(template_path,Creep):
    userFunc = Creep[0]    #Creep
    method = Creep[1]      #User-defined
    model = Creep[2]       #RCC-q0
    datasource = Creep[3]  #fitted_by_2023Test
    if model == "NB_SH-q0":
        updated_contentCE = CreepNB_SH_q0()
    elif model == "NB_TH-q0":
        updated_contentCE = CreepNB_TH_q0()
    elif model == "NBPN_q0":
        updated_contentCE = CreepNBPN_q0()
    elif model == "RCC_q0":
        updated_contentCE = CreepRCC_q0()
    elif model == 'ASME_225Cr1Mo':
        updated_contentCE = ''            # 后面不再用
        template_content = Creep_ASME225Cr1Mo(open(template_path).read())
        return template_content            # 直接回主流程

    else:
        print(u"不支持的蠕变子程序类型：{}".format(model).encode('GB18030'))
        return 0
    # 提取参数列表
    params_list = Creep[-1]
    # 生成参数行
    param_lines = []
    rows = len(params_list)     #行数
    columns = len(params_list[0]) if rows > 0 else 0  # 获取数据列数

    # 续行符计数器 (1-9循环)
    cont_num = 1
    for i in range(rows):
        entry = params_list[i]
        # 转换为D0格式字符串
        param_strs = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry]
        line_part = ", ".join(param_strs)
        # 分块处理每行最多5个元素
        chunks = [param_strs[j:j + 5] for j in range(0, len(param_strs), 5)]
        for chunk_idx, chunk in enumerate(chunks):
            line_part = ", ".join(chunk)
            # 首行处理
            if i == 0 and chunk_idx == 0:
                line = "     {} (/{},".format(cont_num, line_part)
                # 最后一块处理
            elif i == rows - 1 and chunk_idx == len(chunks) - 1:
                line = "     {} {}/)".format(cont_num, line_part)
            # 中间块处理
            else:
                line = "     {} {},".format(cont_num, line_part)
            param_lines.append(line)
            cont_num = cont_num % 9 + 1  # 续行符循环1-9

    # 组合参数块
    params_block = "\n".join(param_lines)
    # 生成 Fortran 数组声明
    array_declaration = (
        "      REAL*8, DIMENSION({0}, {1}) :: CreepCEpara_q0= RESHAPE(\n"
        "{2},\n"
        "     {3} (/{0}, {1}/))".format(columns, rows, params_block, cont_num) #先列后行
    )

    # 读取 Fortran 模板
    with open(template_path, 'r') as f:
        template_content = f.read()
    # 替换 `{{CreepCE_X}}` 为实际的 creep_model，例如 "CreepRCC_q0"
    template_content = template_content.replace("{{CreepCE_X}}", "Creep{}".format(model))

    # 替换 `{{MTYPE}}` 为实际的材料类型编号
    # if material in ["225Cr-1Mo", "9Cr-1Mo-V"]:
    #     MTYPE = 3
    # elif material == "800H":
    #     MTYPE = 2
    # else:
    #     MTYPE = 1
    # template_content = template_content.replace("{{MTYPE}}", str(MTYPE))

    # **使用正则表达式找到 `!CREEP_PARAMS_START` 和 `!CREEP_PARAMS_END` 之间的内容**
    pattern = r'(!CREEP_PARAMS_START\s*).*?(!CREEP_PARAMS_END)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    updated_content = re.sub(
        pattern,
        "\\1\n{}\n\\2".format(array_declaration),
        template_content,
        flags=re.DOTALL
    )


    # **使用正则表达式找到插入蠕变本构的地方**
    pattern1 = r'(!CreepCE_start\s*).*?(!CreepCE_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    updated_content1 = re.sub(
        pattern1,
        "\\1\n{}\n\\2".format(updated_contentCE),
        updated_content,
        flags=re.DOTALL
    )
    return updated_content1
def generate_user_Plastic(prev_fortran_txt, Plastic_row):
    """
    prev_fortran_txt : 前面几步已经拼好的整段 Fortran 文本
    Plastic_row      : 形如
        ['Plastic','User','ASME_225Cr1Mo','ver2021', []]
    """
    model = Plastic_row[2]        # 'ASME_225Cr1Mo'
    version = Plastic_row[3]      # 'ver2021' 这里只先不细分

    # 目前只支持 2.25Cr-1Mo 这一个模板，后面想扩也很容易
    if model == 'ASME_225Cr1Mo':
        tpl_path = os.path.join(
            script_dir,
            r'fortranBase\2.25Cr-1Mo_Uhard_Creep.for'
        )
    else:
        raise ValueError('暂不支持的 UHARD 模型: {}'.format(model))

    with open(tpl_path, 'r') as f:
        whole_tpl = f.read()

    # --- 用正则把 UHARD 子程序抠出来 --------------------
    #   ^\s*SUBROUTINE\s+UHARD   …   ^\s*END\s*$   （跨行、大小写无关）
    m = re.search(r'^\s*SUBROUTINE\s+UHARD.*?^\s*END\s*$',
                  whole_tpl, flags=re.S | re.I | re.M)
    if not m:
        raise RuntimeError('没在模板里找到 UHARD 子程序')

    uhard_sub = m.group(0).strip()          # 先拿到原始文本
    uhard_sub = _fix_block_spacing(uhard_sub)   # ← 再做缩进/对齐修正

    # ── 3. 拼回主文件 ─────────────────────────────────────
    merged = prev_fortran_txt.rstrip() + '\n\n' + uhard_sub + '\n'
    return merged

# 选用哪个蠕变本构方程插入
def CreepNB_SH_q0():
    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/Creep_User_defined/CreepNB_SH_q0.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path_Direct = os.path.join(script_dir, template_name)
    with open(template_path_Direct, 'r') as f:
        CEtemplate_content = f.read()
    return CEtemplate_content
def CreepNB_TH_q0():
    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/Creep_User_defined/CreepNB_TH_q0.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path_Direct = os.path.join(script_dir, template_name)
    with open(template_path_Direct, 'r') as f:
        CEtemplate_content = f.read()
    return CEtemplate_content
def CreepNBPN_q0():
    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/Creep_User_defined/CreepNBPN_q0.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path_Direct = os.path.join(script_dir, template_name)
    with open(template_path_Direct, 'r') as f:
        CEtemplate_content = f.read()
    return CEtemplate_content
def CreepRCC_q0():
    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/Creep_User_defined/CreepRCC_q0.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path_Direct = os.path.join(script_dir, template_name)
    with open(template_path_Direct, 'r') as f:
        CEtemplate_content = f.read()
    return CEtemplate_content
def Creep_ASME225Cr1Mo(tpl_txt):
    """tpl_txt 已是整段模板文本，返回替换后的文本"""
    ext = open(os.path.join(script_dir,
            r'fortranBase/2.25Cr-1Mo_Uhard_Creep.for')).read()

    # 把外部文件里的 SUBROUTINE CREEP(...) … END 抠出来
    import re
    m = re.search(r'^\s*SUBROUTINE\s+CREEP\b.*?^\s*END\s*$',
              ext, flags=re.I | re.M | re.S)
    if not m:
        raise RuntimeError('外部文件里找不到 SUBROUTINE CREEP')

    creep_txt = m.group(0).strip()   # ① 先拿到原始子程序文本
    creep_txt = _fix_block_spacing(creep_txt)   # ② 再做缩进/对齐修正     # ← 同理
    # 用它“顶掉”模板里原来的第一段 CREEP
    tpl_txt = re.sub(r'^\s*SUBROUTINE\s+CREEP\b.*?^\s*END\s*$',
                     creep_txt, tpl_txt, count=1, flags=re.I|re.S|re.M)

    # 再把 {{CreepCE_X}} 整体删掉（或改成 "CREEP" ）
    tpl_txt = tpl_txt.replace('{{CreepCE_X}}', 'CREEP')

    # 蠕变参数区清空
    tpl_txt = re.sub(r'(!CREEP_PARAMS_START\s*).*?(!CREEP_PARAMS_END)',
                     r'\1\n\2', tpl_txt, flags=re.S)
    return tpl_txt



# 蠕变损伤分数计算
def generate_user_CreepRuptureLife(CreepFortran,user_Tr):
    userFunc = user_Tr[0]    #user_CreepRuptureLife
    method = user_Tr[1]      #Larson_Miller 或者 Direct
    datasource = user_Tr[2]  #fitted_by_2023Test
    shuju = user_Tr[-1]
    if method == "Direct":
        updated_content1 = user_CreepRuptureLife_Direct(shuju)
    elif method == "Larson_Miller":
        updated_content1 = user_CreepRuptureLife_Larson_Miller(shuju)
    # 写入蠕变损伤计算使用的是哪种方法
    template_content_all = CreepFortran.replace("{{CRDAMAGE}}", "CRDAMAGE_{}".format(method))
    # **使用正则表达式找到需要插入内容的地方**
    pattern = r'(!Creep_Damage_start\s*).*?(!Creep_Damage_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    updated_content = re.sub(
        pattern,
        "\\1{}\n\\2".format(updated_content1),
        template_content_all,
        flags=re.DOTALL
    )
    return updated_content
def user_CreepRuptureLife_Direct(shuju):
    params_list = shuju

    # 提取每个子列表的第一个元素为温度列表
    TEMP_list = [sublist[0] for sublist in params_list]
    # 提取每个子列表的剩余元素（从索引1开始到末尾）为寿命-应力列表
    Sr = [sublist[1:] for sublist in params_list]
    target_length = len(Sr[0])
    Sr = [sublist + [1] * (target_length - len(sublist)) if len(sublist) < target_length else sublist for sublist in Sr]

    # 生成温度参数行
    TEMP_list_lines = []
    cont_num1 = 1
    entry1 = TEMP_list
    # 转换为D0格式字符串
    param_strs1 = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry1]
    line_part1 = ", ".join(param_strs1)
    # 分块处理每行最多5个元素
    chunks1 = [param_strs1[j:j + 5] for j in range(1, len(param_strs1), 5)]
    for chunk_idx, chunk in enumerate(chunks1):
        line_part1 = ", ".join(chunk)
        # 首行处理
        if chunk_idx == 0:
            line1 = "      DATA TEMP_list/{},".format(line_part1)
            cont_num1 = 0
        # 最后一块处理
        elif chunk_idx == len(chunks1) - 1:
            line1 = "     {} {}/".format(cont_num1, line_part1)
        # 中间块处理
        else:
            line1 = "     {} {},".format(cont_num1, line_part1)
        TEMP_list_lines.append(line1)
        cont_num1 = cont_num1 % 9 + 1  # 续行符循环1-9

    # 生成寿命-应力参数行
    Sr_lines = []
    rows = len(Sr)  # 获取数据行数
    # 续行符计数器 (1-9循环)
    cont_num = 0
    for i in range(rows):
        entry = Sr[i]
        # 转换为D0格式字符串
        param_strs = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry]
        line_part = ", ".join(param_strs)
        # 分块处理每行最多5个元素
        chunks = [param_strs[j:j + 5] for j in range(0, len(param_strs), 5)]
        for chunk_idx, chunk in enumerate(chunks):
            line_part = ", ".join(chunk)
            # 首行处理
            if i == 0 and chunk_idx == 0:
                line = "      DATA Sr/{},".format(line_part)
            # 最后一块处理
            elif i == rows-1 and chunk_idx == len(chunks) - 1:
                line = "     {} {}/".format(cont_num, line_part)
            # 中间块处理
            else:
                line = "     {} {},".format(cont_num, line_part)
            Sr_lines.append(line)
            cont_num = cont_num % 9 + 1  # 续行符循环1-9

    # 组合参数块
    params_block1 = "\n".join(TEMP_list_lines)
    params_block = "\n".join(Sr_lines)
    # 生成 Fortran 数组声明
    array_declaration = ("{}\n"
        "{}".format(params_block1,params_block)  # 先列后行
    )

    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/user_CreepRuptureLife/Direct.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path_Direct = os.path.join(script_dir, template_name)
    with open(template_path_Direct, 'r') as f:
        template_content = f.read()

    # 替换 `{{}}` 为实际的值
    template_content = template_content.replace("{{TEMP_list}}", "{}".format(len(TEMP_list)-1))
    template_content = template_content.replace("{{rows}}", "{}".format(len(Sr[0])))
    template_content = template_content.replace("{{columns}}", "{}".format(len(TEMP_list)))

    # **使用正则表达式找到 `!Direct_start` 和 `!Direct_end` 之间的内容**
    pattern = r'(!Direct_start\s*).*?(!Direct_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    updated_content = re.sub(
        pattern,
        "\\1\n{}\n\\2".format(array_declaration),
        template_content,
        flags=re.DOTALL
    )
    return updated_content
def user_CreepRuptureLife_Larson_Miller(shuju):
    params_list = shuju
    # 生成参数行
    param_lines = []
    rows = len(params_list)  # 行数
    columns = len(params_list[0]) if rows > 0 else 0  # 获取数据列数

    # 续行符计数器 (1-9循环)
    cont_num = 1
    for i in range(rows):
        entry = params_list[i]
        # 转换为D0格式字符串
        param_strs = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry]
        line_part = ", ".join(param_strs)
        # 分块处理每行最多3个元素
        chunks = [param_strs[j:j + 3] for j in range(0, len(param_strs), 3)]
        for chunk_idx, chunk in enumerate(chunks):
            line_part = ", ".join(chunk)
            # 首行处理
            if i == 0 and chunk_idx == 0:
                line = "     {} (/{},".format(cont_num, line_part)
                # 最后一块处理
            elif i == rows - 1 and chunk_idx == len(chunks) - 1:
                line = "     {} {}/)".format(cont_num, line_part)
            # 中间块处理
            else:
                line = "     {} {},".format(cont_num, line_part)

            param_lines.append(line)
            cont_num = cont_num % 9 + 1  # 续行符循环1-9

    # 组合参数块
    params_block = "\n".join(param_lines)
    # 生成 Fortran 数组声明
    array_declaration = (
        "      LMpara=\n"
        "{}\n".format(params_block)  # 先列后行
    )

    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/user_CreepRuptureLife/Larson_Miller.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path_Direct = os.path.join(script_dir, template_name)
    with open(template_path_Direct, 'r') as f:
        template_content = f.read()

    # **使用正则表达式找到需要插入的地方**
    pattern = r'(!larson_miller_start\s*).*?(!larson_miller_end)'

    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    updated_content = re.sub(
        pattern,
        "\\1\n{}\n\\2".format(array_declaration),
        template_content,
        flags=re.DOTALL
    )
    return updated_content


# 疲劳损伤分数计算
def generate_user_FatigueLife(user_TrFortran,user_Nd):
    global updated_content1
    userFunc = user_Nd[0]  # user_FatigueLife
    method = user_Nd[1]  # Direct or Langer or Manson_Coffin
    datasource = user_Nd[2]  # ASME or fittedASME
    shuju = user_Nd[-1]
    if method == "Direct":
        updated_content1 = user_FatigueLife_Direct(shuju)
    elif method == "Langer":
        updated_content1 = user_FatigueLife_Langer(shuju)
    elif method == "Manson_Coffin":
        updated_content1 = 1

    # 在主模板中写入疲劳寿命确定使用的是哪种方法
    template_content_all = user_TrFortran.replace("{{user_FatigueLife_method}}", "{}".format(method))
    # 将生成的子模版插入主模板中
    # **使用正则表达式找到 `!Fatigue_life_model_start` 和 `!Fatigue_life_model_end` 之间的内容**
    pattern = r'(!Fatigue_life_model_start\s*).*?(!Fatigue_life_model_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    updated_content = re.sub(
        pattern,
        "\\1{}\n\\2".format(updated_content1),
        template_content_all,
        flags=re.DOTALL
    )
    return updated_content
def user_FatigueLife_Direct(shuju):
    params_list = shuju
    # 生成参数行
    param_lines = []
    rows = len(params_list)  # 行数
    columns = len(params_list[0]) if rows > 0 else 0  # 获取数据列数

    # 续行符计数器 (1-9循环)
    cont_num = 1
    for i in range(rows):
        entry = params_list[i]
        # 转换为D0格式字符串
        param_strs = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry]
        line_part = ", ".join(param_strs)
        # 分块处理每行最多5个元素
        chunks = [param_strs[j:j + 5] for j in range(0, len(param_strs), 5)]
        for chunk_idx, chunk in enumerate(chunks):
            line_part = ", ".join(chunk)
            # 首行处理
            if i == 0 and chunk_idx == 0:
                line = "     {} (/{},".format(cont_num, line_part)
                # 最后一块处理
            elif i == rows - 1 and chunk_idx == len(chunks) - 1:
                line = "     {} {}/)".format(cont_num, line_part)
            # 中间块处理
            else:
                line = "     {} {},".format(cont_num, line_part)

            param_lines.append(line)
            cont_num = cont_num % 9 + 1  # 续行符循环1-9

    # 组合参数块
    params_block = "\n".join(param_lines)
    # 生成 Fortran 数组声明
    array_declaration = (
        "      REAL*8, DIMENSION({0}, {1}) :: SN= RESHAPE(\n"
        "{2},\n"
        "     {3} (/{0}, {1}/))".format(columns, rows, params_block, cont_num + 1)  # 先列后行
    )

    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/user_Fatigue_Life/DirectModel.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path_Direct = os.path.join(script_dir, template_name)
    with open(template_path_Direct, 'r') as f:
        template_content = f.read()

    # 替换 `{{strain_range_num}}` 为实际的应变范围或寿命的维数
    template_content = template_content.replace("{{strain_range_num}}", "{}".format(rows-1))
    template_content = template_content.replace("{{temps_num}}", "{}".format(columns-1))

    # **使用正则表达式找到 `!CREEP_PARAMS_START` 和 `!CREEP_PARAMS_END` 之间的内容**
    pattern = r'(!Fatigue_life_Directmodel_start\s*).*?(!Fatigue_life_Directmodel_end)'

    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    updated_content = re.sub(
        pattern,
        "\\1{}\n\\2".format(array_declaration),
        template_content,
        flags=re.DOTALL
    )
    return updated_content
def user_FatigueLife_Langer(shuju):
    params_list = shuju
    # 生成参数行
    param_lines = []
    rows = len(params_list)  # 行数
    columns = len(params_list[0]) if rows > 0 else 0  # 获取数据列数
    # 续行符计数器 (1-9循环)
    cont_num = 1
    for i in range(rows):
        entry = params_list[i]
        # 转换为D0格式字符串
        param_strs = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry]
        line_part = ", ".join(param_strs)
        # 分块处理每行最多5个元素
        chunks = [param_strs[j:j + 3] for j in range(0, len(param_strs), 3)]
        for chunk_idx, chunk in enumerate(chunks):
            line_part = ", ".join(chunk)
            # 首行处理
            if i == 0 and chunk_idx == 0:
                line = "     {} (/{},".format(cont_num, line_part)
                # 最后一块处理
            elif i == rows - 1 and chunk_idx == len(chunks) - 1:
                line = "     {} {}/)".format(cont_num, line_part)
            # 中间块处理
            else:
                line = "     {} {},".format(cont_num, line_part)

            param_lines.append(line)
            cont_num = cont_num % 9 + 1  # 续行符循环1-9

    # 组合参数块
    params_block = "\n".join(param_lines)
    # 生成 Fortran 数组声明
    array_declaration = (
        "      REAL*8, DIMENSION({0}, {1}) :: Lpara= RESHAPE(\n"
        "{2},\n"
        "     {3} (/{0}, {1}/))".format(columns, rows, params_block, cont_num)  # 先列后行
    )

    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/user_Fatigue_Life/LangerModel.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path_Langer = os.path.join(script_dir, template_name)
    with open(template_path_Langer, 'r') as f:
        template_content = f.read()

    # 替换 `{{strain_range_num}}` 为实际的 应变范围或寿命的 维数
    template_content = template_content.replace("{{para}}", "{}".format(columns - 1))
    template_content = template_content.replace("{{temps}}", "{}".format(rows))

    # **使用正则表达式找到 `!CREEP_PARAMS_START` 和 `!CREEP_PARAMS_END` 之间的内容**
    pattern = r'(!langer_start\s*).*?(!langer_end)'

    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    updated_content = re.sub(
        pattern,
        "\\1{}\n\\2".format(array_declaration),
        template_content,
        flags=re.DOTALL
    )
    return updated_content


# 蠕变-疲劳损伤包络线
def generate_user_CreepFatigueInteractionCriterion(user_NdFortran,user_CFInter):
    pointCreep = user_CFInter[-1][0][0]
    pointFatigue = user_CFInter[-1][0][1]
    k1, b1, k2, b2, k3 = calculate_lines(pointCreep, pointFatigue)
    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/user_CreepFatigueInteractionCriterion/is_above.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path_Direct = os.path.join(script_dir, template_name)
    with open(template_path_Direct, 'r') as f:
        template_content = f.read()
    # 替换k和b
    template_content = template_content.replace("{{k1}}", "{}".format(k1))
    template_content = template_content.replace("{{b1}}", "{}".format(b1))
    template_content = template_content.replace("{{k2}}", "{}".format(k2))
    template_content = template_content.replace("{{b2}}", "{}".format(b2))
    updated_content1 = template_content.replace("{{k0}}", "{}".format(k3))

    # 读入已连续修改的主模板
    template_content_all = user_NdFortran

    # 将生成的子模版插入主模板中
    # **使用正则表达式找到 `!Creep_Fatigue_Enevlope_start` 和 `!Fatigue_life_model_end` 之间的内容**
    pattern = r'(!Creep_Fatigue_Enevlope_start\s*).*?(!Creep_Fatigue_Enevlope_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    updated_content = re.sub(
        pattern,
        "\\1{}\n\\2".format(updated_content1),
        template_content_all,
        flags=re.DOTALL
    )
    return updated_content
def calculate_lines(x0, y0):
    # 计算第一条线（到纵轴点 (0,1)）蠕变损伤线
    try:
        k1 = (y0 - 1.0) / x0 if x0 != 0 else None
        b1 = 1.0
    except ZeroDivisionError:
        k1, b1 = None, None
    # 计算第二条线（到横轴点 (1,0)）
    try:
        k2 = (0.0 - y0) / (1.0 - x0) if (1.0 - x0) != 0 else None
        b2 = 0.0 - k2 * 1.0  # 通过点(1,0)计算截距
    except ZeroDivisionError:
        k2, b2 = None, None
    k3 = y0/x0
    return k1, b1, k2, b2, k3

# 等效应力模型和设计系数
def generate_user_RepresentativeStress(user_CFInterFortran,user_EquivS):
    method = user_EquivS[1]
    if method == "RCCmethod(Hayhurst)":
        SF = user_EquivS[2][0][0]
        updated_content1 = RCC(SF,user_CFInterFortran)
    else:
        SF = user_EquivS[2][0][0]
        C = user_EquivS[2][0][1]
        MTYPE = user_EquivS[2][0][2]
        updated_content1 = ASME(SF,C,MTYPE,user_CFInterFortran)
    return updated_content1
def RCC(SF,user_CFInterFortran):
    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/user_RepresentativeStress/RCC.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path_Direct = os.path.join(script_dir, template_name)
    with open(template_path_Direct, 'r') as f:
        template_content = f.read()
    # 读入已连续修改的主模板
    template_content_all = user_CFInterFortran
    template_content_all = template_content_all.replace("{{SF}}", "{}".format(SF))
    template_content_all = template_content_all.replace("{{REP_SQ}}", "RCC_SQ")
    # 将生成的子模版插入主模板中
    # **使用正则表达式找到 `!REP_SQ_start` 和 `!Creep_Damage_end` 之间的内容**
    pattern =r'(!REP_SQ_start\s*).*?(!REP_SQ_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    updated_content = re.sub(
        pattern,
        "\\1{}\n\\2".format(template_content),
        template_content_all,
        flags=re.DOTALL
    )
    return updated_content
def ASME(SF,C,MTYPE,user_CFInterFortran):
    # 获取当前脚本所在目录的绝对路径
    template_name = "fortranBase/user_RepresentativeStress/ASME.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录）
    template_path_Direct = os.path.join(script_dir, template_name)
    with open(template_path_Direct, 'r') as f:
        template_content = f.read()
    if C in (0, 0.16, 0.24):
        template_content = template_content.replace("{{MTYPE}}", "{}".format(MTYPE))
        template_content = template_content.replace("{{C}}", "{}".format(0.24))
    else:
        template_content = template_content.replace("{{MTYPE}}", "{}".format(99))
        template_content = template_content.replace("{{C}}", "{}".format(C))
    # 读入已连续修改的主模板
    template_content_all = user_CFInterFortran
    template_content_all = template_content_all.replace("{{SF}}", "{}".format(SF))
    template_content_all = template_content_all.replace("{{REP_SQ}}", "ASME_SQ")
    # 将生成的子模版插入主模板中
    # **使用正则表达式找到 `!REP_SQ_start` 和 `!Creep_Damage_end` 之间的内容**
    pattern = r'(!REP_SQ_start\s*).*?(!REP_SQ_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    updated_content = re.sub(
        pattern,
        "\\1{}\n\\2".format(template_content),
        template_content_all,
        flags=re.DOTALL
    )
    return updated_content

if __name__ == '__main__':
    JSON_PATH = (r'D:\SIMULIA\EstProducts\2023\win_b64\code\python2.7'
                r'\lib\abaqus_plugins\STPM_test1034\MaterialData.json')

    # ===== ② 读取数据库，只要 2.25Cr1Mo 这一支 =====
    with open(JSON_PATH, 'r') as fp:
        all_mat_db = json.load(fp)

    MAT_KEY = '2.25Cr1Mo'            # JSON 里外层键
    try:
        mat_json = all_mat_db[MAT_KEY]
    except KeyError:
        raise RuntimeError('MaterialData.json 里没有 “2.25Cr1Mo” 这一条!')

    # ===== ③ 依照界面勾选, 只保留 √ 的那两条 =============
    SEL_MODEL = 'ASME_225Cr1Mo'      # 勾选的子模型
    SEL_VER   = 'ver2021'

    # ---- 3.1  Creep → User_defined -----------------------
    creep_user = mat_json.setdefault('Creep', {}) \
                        .setdefault('User_defined', {})

    # 把同级的 NB_TH_q0 / NB_SH_q0 … 都干掉，只留 ASME_225Cr1Mo
    for sub in list(creep_user.keys()):
        if sub != SEL_MODEL:
            creep_user.pop(sub)

    creep_user.setdefault(SEL_MODEL, {}) \
            .setdefault(SEL_VER, [])       # 保证路径存在 (空列表即可)

    # ---- 3.2  Plastic → User -----------------------------
    plast_user = mat_json.setdefault('Plastic', {}) \
                        .setdefault('User', {})

    for sub in list(plast_user.keys()):
        if sub != SEL_MODEL:
            plast_user.pop(sub)

    plast_user.setdefault(SEL_MODEL, {}) \
            .setdefault(SEL_VER, [])

    # ===== ④ 调用你的主入口 ================================
    aimMaterialName = 'M225Cr1Mo'   # Abaqus Model 里的材料名
    UVARMnum        = 33
    SDVnum          = 0

    pre_materialImport_main(mat_json,
                            aimMaterialName,
                            UVARMnum,
                            SDVnum)

