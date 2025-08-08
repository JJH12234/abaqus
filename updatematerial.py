# -*- coding: utf-8 -*-
#
# 该文件定义了用于处理Abaqus材料属性和Fortran子程序生成的Python脚本。
#
"""
Created on Tue Mar  4 16:58:27 2025

@author: mrvoid
"""
# 导入Abaqus相关的模块和常量。
from abaqus import *
# 导入Abaqus内置的常量，例如YES, NO, CANCEL等。
from abaqusConstants import *
# 导入OrderedDict，用于保持字典中元素的插入顺序。
from collections import OrderedDict
# 导入os模块，用于操作系统相关的操作，例如路径处理。
import os
# 导入re模块，用于正则表达式操作。
import re
# 导入inspect模块，用于获取有关活动对象的信息，例如当前文件的路径。
import inspect
import json
# 获取当前脚本的文件名。
filename = inspect.getframeinfo(inspect.currentframe()).filename
# 获取当前脚本所在的目录。
script_dir     = os.path.dirname(os.path.abspath(filename))
#
# ---------- 缩进常量 ----------
# 定义Fortran代码块中不同级别的缩进常量。
_IND1      = '      '     # 一级缩进：6 个空格，通常用于Fortran代码的起始行。
_IND_STEP  = '   '        # IF 嵌套每增加一级时，额外增加的缩进空格数（3个空格）。
_IND_CONT  = '     '      # Fortran行续行符 '&' 后面的缩进，使用 5 个空格。

# ---------- 正则 ----------
# 定义用于匹配Fortran代码中不同模式的正则表达式。
_RE_COMMENT   = re.compile(r'^[cC\*!]')                       # 匹配以 'c', 'C', '*', '!' 开头的行，表示Fortran注释行。
_RE_INC_MIS   = re.compile(r'^[cC]\s*INCLUDE\s+\'ABA', re.I)  # 匹配不规范的 INCLUDE 'ABA_PARAM.INC' 语句，re.I表示忽略大小写。
_RE_PARAM     = re.compile(r'^\s*(temp\d+|x_[A-Za-z]\w*)\s*=', re.I) # 匹配Fortran中的参数定义行，例如 temp1 = ... 或 x_var = ...。
_RE_IFLINE    = re.compile(r'\b(if\b.*then|else\b|elseif\b|end\s*if\b)', re.I) # 匹配Fortran的IF、ELSE、ELSEIF、END IF语句。

# 定义一个函数，用于规范化Fortran代码的缩进和注释对齐。
def _fix_block_spacing(txt):
    """
    把一段 UHARD / CREEP Fortran 代码的缩进、注释对齐到示范格式
    该函数旨在将用户定义的Fortran子程序（如UHARD或CREEP）代码格式化为标准样式。
    它处理缩进、注释和续行符，以提高代码的可读性和一致性。
    """
    out   = []                    # 用于存储格式化后的代码行。
    depth = 0                     # 当前 IF 嵌套的层数，用于控制缩进深度。

    # 遍历输入的Fortran代码的每一行。
    for raw in txt.splitlines():
        ln  = raw.rstrip()        # 移除行尾的空白字符，但不动结尾的续行符 '&'。
        lns = ln.lstrip()         # 移除行首的空白字符，用于判断行类型。

        # ---------- 0. 修错 INCLUDE ----------
        # 检查当前行是否是需要修正的 INCLUDE 语句。
        if _RE_INC_MIS.match(ln):
            # 如果匹配，则替换为规范的 INCLUDE 语句并添加一级缩进。
            out.append(_IND1 + "INCLUDE 'ABA_PARAM.INC'")
            continue # 处理完当前行，跳到下一行。

        # ---------- 1. 注释行 ----------
        # 检查当前行是否是Fortran注释行。
        if _RE_COMMENT.match(ln):
            out.append(lns)        # 注释行顶格输出，不添加额外缩进。
            continue # 处理完当前行，跳到下一行。

        # ---------- 2. 续行符 & ----------
        # 检查当前行是否以续行符 '&' 开头。
        if lns.startswith('&'):
            out.append(_IND_CONT + lns) # 添加续行符的特定缩进。
            continue # 处理完当前行，跳到下一行。

        # ---------- 3. IF / ELSE / ELSEIF / END IF ----------
        # 检查当前行是否包含IF、ELSE、ELSEIF或END IF语句。
        m_if = _RE_IFLINE.search(ln)
        if m_if:
            # 根据当前嵌套深度计算缩进。
            indent = _IND1 + _IND_STEP * depth
            # 获取匹配到的关键字（例如 'if', 'else', 'end if'）。
            kw = m_if.group(1).lower()

            if kw.startswith('end'):          # 如果是 END IF 语句。
                depth = max(depth - 1, 0)     # 嵌套深度减一，确保不小于0。
                indent = _IND1 + _IND_STEP * depth # 重新计算缩进。
                out.append(indent + lns)      # 添加缩进后的行。
                continue # 处理完当前行，跳到下一行。

            if kw.startswith('else'):         # 如果是 ELSE / ELSEIF 语句。
                depth = max(depth - 1, 0)     # 嵌套深度减一，因为ELSE/ELSEIF与IF在同一层级。
                indent = _IND1 + _IND_STEP * depth # 重新计算缩进。
                out.append(indent + lns)      # 添加缩进后的行。
                if lns.endswith('then'):      # 如果是 ELSEIF ... THEN 语句，后续代码需要增加缩进。
                    depth += 1                # 嵌套深度加一。
                continue # 处理完当前行，跳到下一行。

            # 处理普通的 IF … THEN 语句。
            out.append(indent + lns)          # 添加缩进后的行。
            depth += 1                        # 嵌套深度加一。
            continue # 处理完当前行，跳到下一行。

        # ---------- 4. “参数常量行” 仅最外层 6 空格 ----------
        # 检查当前行是否是参数常量定义，并且当前不在IF嵌套内部（depth == 0）。
        if _RE_PARAM.match(ln) and depth == 0:
            out.append(_IND1 + lns)           # 添加一级缩进。
            continue # 处理完当前行，跳到下一行。

        # ---------- 5. 其它可执行语句 ----------
        # 处理所有其他类型的可执行语句。
        indent = _IND1 + _IND_STEP * depth    # 根据当前嵌套深度计算缩进。
        out.append(indent + lns)              # 添加缩进后的行。

    return '\n'.join(out) # 将所有处理后的行连接成一个字符串并返回。

# 定义一个函数，用于获取当前Abaqus视口关联的模型。
def get_current_model():
    """获取当前视口关联的模型"""
    flag=None # 初始化标志变量。
    viewport = session.currentViewportName # 获取当前激活的视口名称。
    modelname=session.sessionState[viewport]['modelName'] # 从会话状态中获取当前视口显示的模型名称。
    # 检查模型名称是否包含 'Model-0'，这是Abaqus的默认模型名称。
    if 'Model-0' in modelname:
        # 如果是 'Model-0'，则弹出警告，建议用户不要直接编辑。
        flag=getWarningReply(
            u'警告： 不推荐用户自行编辑Model-0！\n YES以强制编辑; No建立副本并编辑;'.encode('GB18030'), (YES,NO,CANCEL))
    # 根据用户的选择进行不同的操作。
    if flag==NO:
        newname=modelname.replace('Model-0','NewModel') # 如果用户选择NO，则创建新模型名称。
        if newname in mdb.models.keys(): # 检查新模型名称是否已存在。
            newname=newname+'-Copy' # 如果存在，则添加 '-Copy' 后缀。
        # 创建一个新模型，并复制 'Model-0' 的内容。
        mdb.Model(name=newname,
                  objectToCopy=mdb.models[modelname])
        a = mdb.models[newname].rootAssembly # 获取新模型的根装配。
        session.viewports[viewport].setValues(displayedObject=a) # 将视口显示对象设置为新模型的装配。
        return mdb.models[newname] # 返回新模型对象。
    elif flag==CANCEL:
        # 如果用户选择CANCEL，则抛出异常。
        raise Exception('User Cancels when edit {mm}'.format(mm=modelname))
        # 注意：这里的 return mdb.models[modelname] 在 raise Exception 之后，实际上不会被执行到。
        return mdb.models[modelname] # 返回原始模型对象（此行不可达）。
    # 如果不是 'Model-0' 或者用户选择YES，则直接返回当前模型。
    return mdb.models[modelname]

# 定义主函数，用于导入材料属性和生成Fortran子程序。
def pre_materialImport_main(jsondata,aimMaterialName,UVARMnum,SDVnum):
    # jsondata 应该是一个多级字典结构，包含材料属性数据。
    m=get_current_model() # 获取当前模型。
    # 检查目标材料名称是否已存在于模型中。
    if aimMaterialName in m.materials:
        pass# 如果材料已存在，则不修改其其他属性，只更新或添加新的属性。
    else:
        try:
            m.Material(name=aimMaterialName) # 如果材料不存在，则新建一个材料。
        except AbaqusNameError as e:
            # 如果材料名称不合法，则抛出异常。
            raise Exception(u"{}不能作为材料名".format(aimMaterialName).encode('GB18030'))
            return 0 # 返回0表示失败。
    mm=m.materials[aimMaterialName] # 获取或创建的材料对象。
    # filtered_data = {k: v for k, v in jsondata.items() if not k.startswith("user_")} #排除子程序用参数
    filtered_data = {} # 用于存储非用户自定义参数的材料数据。
    fortran_data = {}  # 用于存储用户自定义参数（Fortran子程序相关）的数据。
    # 遍历jsondata，将数据分为两类：Fortran相关和非Fortran相关。
    for k, v in jsondata.items():
        if k.startswith("user_"): # 如果键以 "user_" 开头，则认为是Fortran相关参数。
            fortran_data[k] = v
        else:
            filtered_data[k] = v
    data=process_dict(filtered_data) # 将过滤后的数据处理为列表形式，以便后续添加属性。
    # 遍历处理后的数据，为材料添加每个属性。
    for properrow in data:
        addproperty(mm, properrow) # 调用 addproperty 函数添加材料属性。
    # 处理用户自定义输出变量 (UVARM)。
    if UVARMnum>0:
        mm.UserOutputVariables(n=UVARMnum) # 如果UVARM数量大于0，则设置用户输出变量。
    else:
        if getattr(mm,'userOutputVariables',None): # 如果UVARM数量为0且已存在，则删除。
            del mm.userOutputVariables
    # 处理用户自定义状态变量 (SDV)。
    if SDVnum>0:
        mm.UserDefinedField() # 启用用户自定义场。
        mm.Depvar(n=SDVnum)   # 设置状态变量数量。
    else:
        if getattr(mm,'depvar',None): # 如果SDV数量为0且已存在，则删除。
            del mm.depvar
        if getattr(mm,'userDefinedField',None): # 如果用户自定义场已存在，则删除。
            del mm.userDefinedField
    # print(process_dict(fortran_data)) # 打印Fortran相关数据（调试用）。
    # 调用 pre_materialImport 函数，获取Fortran子程序所需的参数。
    Creep, Plastic, user_EquivS,user_Tr,user_Nd, user_CFInter,user_Type = pre_materialImport(jsondata)
    fortran_name=aimMaterialName # 将目标材料名称作为Fortran子程序的名称。
    # 调用 generate_Fortran 函数生成Fortran子程序文件。
    generate_Fortran(Creep,Plastic, user_EquivS,user_Tr,user_Nd, user_CFInter,user_Type,fortran_name)
    return # 函数执行完毕。

# 定义一个递归函数来处理嵌套字典，将其扁平化为列表。
def process_dict(d, path=None):
    # path 参数用于记录当前键的路径，默认为空列表。
    if path is None:
        path = []
    result = [] # 存储扁平化后的结果。
    # 遍历字典中的每个键值对。
    for key, value in d.items():
        if isinstance(value, dict):
            # 如果值是字典，则递归调用自身，并将当前键添加到路径中。
            result.extend(process_dict(value, path + [key]))
        else:
            # 如果值不是字典，则将其作为最终元素添加到结果列表中，包含完整的路径和值。
            result.append(path + [key, value])
    return result # 返回扁平化后的列表。

# 定义一个函数，用于检查材料属性的温度依赖性以及数据列数是否符合预期。
def tDepCheck(property_name, property_type, property_data):
    # 定义属性与期望列数的映射关系。
    # 字典的键是属性名称，值是另一个字典，其中包含不同属性类型对应的期望列数。
    property_columns = {
        'Density': {'Uniform': 1}, # 密度属性，均匀类型期望1列数据。
        'Elastic': {'Isotropic': 2}, # 弹性属性，各向同性期望2列数据。
        'Conductivity': {'Isotropic': 1}, # 传导性属性，各向同性期望1列数据。
        'Specific Heat': {'*':1}, # 比热属性，任何类型期望1列数据。
        'Expansion': {'Isotropic': 1}, # 热膨胀属性，各向同性期望1列数据。
        'Plastic': {'Isotropic': 2, 'User': 1}, # 塑性属性，各向同性期望2列，用户自定义期望1列。
        'Creep': { # 蠕变属性的复杂映射。
            'Strain-Harding': 3, # 应变硬化期望3列。
            'Time-Harding': 3,   # 时间硬化期望3列。
            'Power': 4,          # 幂律期望4列。
            'Time_Power': 4,     # 时间幂律期望4列。
            'User': 0            # 用户自定义蠕变，不期望数据列（或由子程序提供）。
        },
    }
    # 如果属性数据为空，则默认关闭温度依赖性。
    if not property_data:
        return OFF  # 无数据时默认关闭。
    # 获取实际传入数据的列数。
    # 假设 property_data 是一个列表的列表，每个内部列表代表一行数据。
    num_columns = len(property_data[0])
    
    # 获取属性名称对应的期望列数规范。
    columns_spec = property_columns.get(property_name)
    if columns_spec is None:
        return ON  # 如果属性不在映射中，默认开启温度依赖性。
    # 解析期望列数。
    if isinstance(columns_spec, dict):
        # 先尝试获取具体类型（如 'Isotropic', 'User'）的期望列数。
        expected_num = columns_spec.get(property_type)
        # 如果具体类型不存在，则检查是否存在通配符 '*'。
        if expected_num is None and '*' in columns_spec:
            expected_num = columns_spec['*']
    else:
        # 如果 columns_spec 直接是数字（不常见，但作为备用）。
        expected_num = columns_spec
    
    # 如果未找到具体类型或通配符对应的期望列数，默认开启温度依赖性。
    if expected_num is None:
        return ON
    
    # 判断实际数据列数是否与期望列数匹配。
    # Abaqus中，如果数据包含温度依赖性，通常会多一列温度数据。
    # 所以实际列数 = 期望列数 + 1 (如果包含温度)。
    # 这里判断的是实际列数是否等于期望列数 + 1，如果是，则开启温度依赖性。
    return ON if num_columns == expected_num + 1 else OFF

# 定义一个函数，用于向Abaqus材料对象添加具体的属性。
def addproperty(mm, datarow):
    # 从 datarow 中解析属性名称、属性类型和表格数据。
    property_name = datarow[0] # 例如 'Density', 'Elastic'。
    property_type = datarow[1] # 例如 'Uniform', 'Isotropic'。
    table_data = datarow[-1]   # 属性的数值数据，通常是元组的元组。
    # print(table_data) # 调试打印表格数据。
    # 检查表格数据是否为空（例如 ((None,),)）。
    if table_data==((None,),):
        try:
            # 如果数据为空，尝试删除该属性。
            print(u"{0}数据为空，将尝试删除{0}属性".format(unicode(property_name)).encode('GB18030'))
            try:
                # 尝试通过属性名（首字母小写）获取并删除材料对象的属性。
                mmp=getattr(mm,property_name[0].lower() + property_name[1:])
                del mmp # 删除属性。
            except Exception as e:
                # 如果删除失败，打印错误信息。
                print(u'删除{}失败，请管理员检查关键字'.format(unicode(property_name)).encode('GB18030'))
            return 0 # 返回0表示处理失败或已删除。
        except Exception as e:
            print(e) # 打印其他异常信息。
            return 0 # 返回0。
    # 检查属性的温度依赖性。
    tDCflag = tDepCheck(property_name, property_type, table_data)
    try:
        # 尝试将 property_type 转换为 Abaqus 常量（例如 'ISOTROPIC'）。
        tryarg=globals()[property_type.upper().replace(' ', '')]
    except (KeyError , AttributeError):
        # 如果转换失败，则设置为 NONE。
        tryarg=NONE
        # 对于非蠕变属性，如果类型转换失败，则打印警告。
        if property_name!='Creep': # 蠕变属性下'law'参数的常量名比较奇怪， 不直接对应。
            print(u"{pt}不是abaqusConstants".format(pt=unicode(str(property_type)).encode('GB18030').upper().replace(' ', '')))
            print(u"属性{pn}以默认值建立".format(pn=unicode(property_name)).encode('GB18030'))
    # 定义一个处理程序映射，根据属性名称和类型来确定如何调用Abaqus API。
    handler_map = {
        # Density处理
        ('Density', 'Uniform'): { # 密度属性，均匀类型。
            'method': 'Density', # 对应Abaqus材料对象的 Density 方法。
            'args': { # 调用方法所需的参数。
                'distributionType': UNIFORM, # 分布类型为均匀。
                'table': table_data,         # 表格数据。
                'temperatureDependency': tDCflag # 温度依赖性标志。
            }
        },
        ('Density', '*'): { # 密度属性，其他类型（如离散场）。
            'method': 'Density',
            'args': {
                'distributionType': DISCRETE_FIELD, # 分布类型为离散场。
                'fieldName': property_type,         # 场名称。
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        # Creep处理
        ('Creep', 'User_defined'): { # 蠕变属性，用户自定义类型。
            'method': 'Creep',
            'args': {'law': USER, 'table': ()} # 蠕变定律为用户自定义，表格数据为空。
        },
        ('Creep', 'Power'): { # 蠕变属性，幂律类型。
            'method': 'Creep',
            'args': {
                'law': POWER_LAW, # 蠕变定律为幂律。
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Creep', 'Time_Power'): { # 蠕变属性，时间幂律类型。
            'method': 'Creep',
            'args': {
                'law': TIME_POWER_LAW, # 蠕变定律为时间幂律。
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Creep', 'Time-Harding'): { # 蠕变属性，时间硬化类型。
            'method': 'Creep',
            'args': {
                'law': TIME, # 蠕变定律为时间硬化。
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Creep', 'Strain-Harding'): { # 蠕变属性，应变硬化类型。
            'method': 'Creep',
            'args': {
                'law': STRAIN, # 蠕变定律为应变硬化。
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Creep', '*'): { # 蠕变属性，其他通用类型。
            'method': 'Creep',
            'args': {
                'law': tryarg, # 使用之前转换的 Abaqus 常量作为定律。
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        # Plastic处理
        ('Plastic','User'):{ # 塑性属性，用户自定义类型。
            'method': 'Plastic',
            'args': {
                'hardening': USER, # 硬化类型为用户自定义。
                'table': table_data,
            }
        },
        ('Plastic', '*'): { # 塑性属性，其他通用类型。
            'method': 'Plastic',
            'args': {
                'hardening': tryarg, # 使用之前转换的 Abaqus 常量作为硬化类型。
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        # Specific Heat处理
        ('Specific Heat', '*'): { # 比热属性，通用类型。
            'method': 'SpecificHeat', # 对应 Abaqus 材料对象的 SpecificHeat 方法。
            'args': {
                'law': tryarg, #  用于某些比热模型，但通常比热没有 'law' 参数，这里 是一个通用占位符。
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        # 通用类型处理（Elastic, Expansion, Conductivity）
        ('Elastic', '*'): { # 弹性属性，通用类型。
            'method': property_name, # 方法名直接使用属性名，例如 'Elastic'。
            'args': {
                'type':  tryarg, # 弹性类型，例如 'ISOTROPIC'。
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Expansion', '*'): { # 热膨胀属性，通用类型。
            'method': property_name, # 方法名直接使用属性名，例如 'Expansion'。
            'args': {
                'type':  tryarg, # 膨胀类型，例如 'ISOTROPIC'。
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('Conductivity', '*'): { # 传导性属性，通用类型。
            'method': property_name, # 方法名直接使用属性名，例如 'Conductivity'。
            'args': {
                'type':  tryarg, # 传导类型，例如 'ISOTROPIC'。
                'table': table_data,
                'temperatureDependency': tDCflag
            }
        },
        ('*', '*'): { # 捕获所有未明确定义的属性类型，作为通用处理。
            'method': property_name.replace(' ', ''), # 方法名移除空格。
            'args': OrderedDict([ # 使用 OrderedDict 保持参数顺序。
                ('type',  tryarg), # 类型参数。
                ('table', table_data), # 表格数据。
                ('temperatureDependency', tDCflag) # 温度依赖性。
            ])
        }
    }
    
    # 查找合适的处理程序。
    handler = None # 初始化处理程序为 None。
    specific_key = (property_name, property_type) # 具体的属性名和类型组合。
    wildcard_key = (property_name, '*') # 属性名和通配符类型组合。
    any_key = ('*', '*') # 任意属性名和任意类型组合（通用捕获）。
    
    # 优先查找具体的处理程序。
    if specific_key in handler_map:
        handler = handler_map[specific_key]
    # 如果没有具体的，则查找通配符类型的处理程序。
    elif wildcard_key in handler_map:
        handler = handler_map[wildcard_key]
    
    # 如果未找到任何特定的处理程序。
    if not handler:
        #raise ValueError(f"No handler for {property_name}/{property_type}") # 抛出错误（注释掉）。
        # 打印警告信息，表示没有找到对应的处理程序。
        print(u"没有名为{property_name}/{property_type}的属性".format(property_name=property_name,property_type=property_type).encode('GB18030'))
        handler = handler_map[any_key] # 使用通用处理程序。
        method = getattr(mm, handler['method']) # 获取材料对象对应的方法。
        args_dict = handler['args'] # 获取方法参数字典。
        if tryarg==NONE: # 如果类型参数为 NONE。
            del args_dict['type'] # 从参数中删除 'type' 键。
            method(**args_dict) # 调用方法，使用剩余参数。
        else:
            if args_dict: # 如果参数字典不为空。
                # 提取第一个参数的值和剩余参数。
                keys = list(args_dict.keys()) # 获取所有键。
                first_value = args_dict[keys[0]] # 获取第一个键的值。
                remaining_args = {k: args_dict[k] for k in keys[1:]} # 获取剩余参数。
                # 调用时第一个参数作为第一个位置的参数传递，其余作为关键字参数。
                method(first_value, **remaining_args)
        return # 处理完毕，返回。
    # 获取方法名和方法参数。
    method_name = handler['method']
    method_args = handler['args']
    # 检查 mm 对象是否有对应的方法。
    if not hasattr(mm, method_name):
        #raise AttributeError(f"Method '{method_name}' not found in mm object") # 抛出错误（注释掉）。
        # 打印警告信息，表示在材料对象中没有找到对应的方法。
        print(u"方法'{method_name}'在mm对象中没有找到".format(method_name=method_name).encode('GB18030'))
    # 如果类型参数为 NONE 且方法参数中包含 'type' 键，则删除 'type' 键。
    if tryarg==NONE and 'type' in method_args: # 注意这里直接检查 'type' in method_args，而不是 hasattr(method_args, 'type')。
        del method_args['type']
    method = getattr(mm, method_name) # 获取材料对象对应的方法。
    method(**method_args) # 调用方法，使用解包的关键字参数。


# 对传入的数据进行预处理并分类
def pre_materialImport(jsondata):
    # jsondata 应该是一个多级字典结构，包含所有材料相关数据。
    # 过滤出以 "user_" 开头的数据，以及其他数据。
    filtered_data = {k: v for k, v in jsondata.items()
                     if k.startswith("user_") or k.startswith("Creep") or k.startswith("Plastic")} #仅需要传入子程序的参数

# 过滤原始数据，只保留键以 "Creep" 或 "Plastic" 开头的数据
    # 递归处理字典，将其扁平化为一个列表，方便后续分类和提取
    flat_data = process_dict(filtered_data)
    # 初始化用于存储不同类型数据的空列表
    Creep = []
    Plastic = []
    user_RepresentativeStress = []
    user_CreepRuptureLife = []
    user_FatigueLife = []
    user_CreepFatigueInteractionCriterion = []
    user_AnalysisType = []
    # 遍历扁平化后的数据列表，根据其第一个元素（类型标识符）进行分类
    for i in range(0, len(flat_data)):
        # 如果数据项的第一个元素是 "Creep" 且第二个元素是 'User_defined'，则将其赋值给 Creep 变量
        if flat_data[i][0] == "Creep" and flat_data[i][1] == 'User_defined':
            Creep = flat_data[i]
        # 如果数据项的第一个元素是 "Plastic" 且第二个元素是 'User'，则将其赋值给 Plastic 变量
        elif flat_data[i][0] == "Plastic" and flat_data[i][1] == 'User':
            Plastic = flat_data[i]
        # 如果数据项的第一个元素是 "user_RepresentativeStress"，则将其赋值给 user_RepresentativeStress 变量
        elif flat_data[i][0] == "user_RepresentativeStress":
            user_RepresentativeStress = flat_data[i]
        # 如果数据项的第一个元素是 "user_CreepRuptureLife"，则将其赋值给 user_CreepRuptureLife 变量
        elif flat_data[i][0] == "user_CreepRuptureLife":
            user_CreepRuptureLife = flat_data[i]
        # 如果数据项的第一个元素是 "user_FatigueLife"，则将其赋值给 user_FatigueLife 变量
        elif flat_data[i][0] == "user_FatigueLife":
            user_FatigueLife = flat_data[i]
        # 如果数据项的第一个元素是 "user_CreepFatigueInteractionCriterion"，则将其赋值给 user_CreepFatigueInteractionCriterion 变量
        elif flat_data[i][0] == "user_CreepFatigueInteractionCriterion":
            user_CreepFatigueInteractionCriterion = flat_data[i]
        # 如果数据项的第一个元素是 "user_AnalysisType"，则将其赋值给 user_AnalysisType 变量
        elif flat_data[i][0] == "user_AnalysisType":
            user_AnalysisType = flat_data[i]
    # 返回分类后的所有数据列表
    return Creep, Plastic, user_RepresentativeStress,user_CreepRuptureLife,\
           user_FatigueLife,user_CreepFatigueInteractionCriterion,\
           user_AnalysisType
# 定义一个递归函数来处理字典，将其扁平化为键值对的列表
def process_dict(d, path=None):
    # 如果 path 为 None，则初始化为空列表，用于存储当前路径
    if path is None:
        path = []
    # 初始化结果列表，用于存储扁平化后的数据
    result = []
    # 遍历字典中的每个键值对
    for key, value in d.items():
        # 如果值是一个字典，则递归调用 process_dict 处理子字典，并将当前键添加到路径中
        if isinstance(value, dict):
            result.extend(process_dict(value, path + [key]))
        # 如果值不是字典，则表示已到达叶子节点，将当前路径、键和值作为一个列表添加到结果中
        else:
            result.append(path + [key, value])
    # 返回扁平化后的结果列表
    return result

# 生成fortran文件的总体思路函数
def generate_Fortran(Creep=[],Plastic=[], user_EquivS=[],user_Tr=[],user_Nd=[], user_CFInter=[],user_Type=[],materialName=''):
    # 获取当前脚本所在目录的绝对路径，用于构建文件路径
    # script_dir 变量应在函数外部定义，通常通过 os.path.dirname(os.path.abspath(__file__)) 获取
    # 这里假设 script_dir 已经可用
    template_name = "fortranBase/M225Cr1MoMuBan.for"
    # 构建模板文件的完整路径（假设模板文件与脚本同目录或在相对路径下）
    template_path = os.path.join(script_dir, template_name)
    # 初始化 Fortran 代码内容变量，用于逐步构建最终的 Fortran 文件
    # 每一个模块接续处理，根据传入的参数决定生成哪些部分
    CreepFortran = '' # 初始化变量，确保在条件不满足时有默认值
    user_TrFortran = '' # 初始化变量
    user_NdFortran = '' # 初始化变量
    user_CFInterFortran = '' # 初始化变量
    user_EquivSFortran = '' # 初始化变量
    user_PlasticFortran = '' # 初始化变量

    # 如果 Creep 参数存在，则生成蠕变子程序 Fortran 代码
    if Creep:
        CreepFortran = generate_creep_subroutine(template_path,Creep)
    # 如果 user_Tr 参数存在，则生成用户定义的蠕变断裂寿命 Fortran 代码
    # 并且将上一步生成的 CreepFortran 作为输入，进行链式处理
    if user_Tr:
        user_TrFortran = generate_user_CreepRuptureLife(CreepFortran,user_Tr)
    # 如果 user_Nd 参数存在，则生成用户定义的疲劳寿命 Fortran 代码
    # 并且将上一步生成的 user_TrFortran 作为输入
    if user_Nd:
        user_NdFortran = generate_user_FatigueLife(user_TrFortran,user_Nd)
    # 如果 user_CFInter 参数存在，则生成用户定义的蠕变疲劳交互准则 Fortran 代码
    # 并且将上一步生成的 user_NdFortran 作为输入
    if user_CFInter:
        user_CFInterFortran = generate_user_CreepFatigueInteractionCriterion(user_NdFortran,user_CFInter)
    # 如果 user_EquivS 参数存在，则生成用户定义的代表应力 Fortran 代码
    # 并且将上一步生成的 user_CFInterFortran 作为输入
    if user_EquivS:
        user_EquivSFortran = generate_user_RepresentativeStress(user_CFInterFortran,user_EquivS)
    # 如果 Plastic 参数存在，则生成用户定义的塑性 Fortran 代码
    # 并且将上一步生成的 user_EquivSFortran 作为输入
    if Plastic:
        user_PlasticFortran = generate_user_Plastic(user_EquivSFortran, Plastic)
    # 如果 Plastic 参数不存在，则 user_PlasticFortran 保持为空字符串
    else:
        user_PlasticFortran = ''
    # 构造最终 Fortran 文件的完整路径
    fortran_file_path='{}\\{}.for'.format(script_dir,materialName)
    # 以写入模式打开文件，将生成的 Fortran 代码写入文件
    with open(fortran_file_path, 'w') as f:
        f.write(user_PlasticFortran)  # 将生成的 Fortran 代码写入文件
    # 打印输出信息，告知用户文件已生成，并使用 GB18030 编码以支持中文
    print(u'{}.for 已输出到 {}'.format(materialName,script_dir).encode('GB18030'))
    # 返回生成的 Fortran 文件的路径
    return fortran_file_path


# 蠕变本构参数传入函数
def generate_creep_subroutine(template_path,Creep):
    # 从 Creep 列表中提取用户函数类型（例如 "Creep"）
    userFunc = Creep[0]    #Creep
    # 从 Creep 列表中提取方法类型（例如 "User-defined"）
    method = Creep[1]      #User-defined
    # 从 Creep 列表中提取模型名称（例如 "RCC-q0"）
    model = Creep[2]       #RCC-q0
    # 从 Creep 列表中提取数据源（例如 "fitted_by_2023Test"）
    datasource = Creep[3]  #fitted_by_2023Test
    # 根据模型名称选择对应的蠕变本构 Fortran 代码片段
    if model == "NB_SH-q0":
        updated_contentCE = CreepNB_SH_q0()
    # 如果模型是 "NB_TH-q0"，则调用相应的函数获取代码
    elif model == "NB_TH-q0":
        updated_contentCE = CreepNB_TH_q0()
    # 如果模型是 "NBPN_q0"，则调用相应的函数获取代码
    elif model == "NBPN_q0":
        updated_contentCE = CreepNBPN_q0()
    # 如果模型是 "RCC_q0"，则调用相应的函数获取代码
    elif model == "RCC_q0":
        updated_contentCE = CreepRCC_q0()
    # 如果模型是 'ASME_225Cr1Mo'，则进行特殊处理
    elif model == 'ASME_225Cr1Mo':
        updated_contentCE = ''            # 后面不再用这个变量，因为直接返回了
        # 读取模板内容并调用 Creep_ASME225Cr1Mo 函数进行处理
        template_content = Creep_ASME225Cr1Mo(open(template_path).read())
        return template_content            # 直接返回处理后的模板内容，结束函数执行

    # 如果模型类型不支持，则打印错误信息并返回 0
    else:
        print(u"不支持的蠕变子程序类型：{}".format(model).encode('GB18030'))
        return 0
    # 提取参数列表，通常是 Creep 列表的最后一个元素
    params_list = Creep[-1]
    # 初始化参数行列表，用于存储格式化后的 Fortran 参数行
    param_lines = []
    # 获取参数列表的行数
    rows = len(params_list)     #行数
    # 获取参数列表的列数，如果行数为0则列数为0
    columns = len(params_list[0]) if rows > 0 else 0  # 获取数据列数

    # 续行符计数器 (1-9循环)，用于 Fortran 代码的续行
    cont_num = 1
    # 遍历参数列表的每一行
    for i in range(rows):
        # 获取当前行的数据
        entry = params_list[i]
        # 将每个参数转换为 Fortran 的 D0 格式字符串（双精度浮点数），非数字类型保持原样
        param_strs = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry]
        # 将当前行的参数用逗号连接成字符串
        line_part = ", ".join(param_strs)
        # 将当前行的参数分块，每块最多5个元素，以适应 Fortran 的行长度限制
        chunks = [param_strs[j:j + 5] for j in range(0, len(param_strs), 5)]
        # 遍历每个参数块
        for chunk_idx, chunk in enumerate(chunks):
            # 将当前块的参数用逗号连接成字符串
            line_part = ", ".join(chunk)
            # 处理首行（第一个参数块的第一行）
            if i == 0 and chunk_idx == 0:
                # 格式化为 Fortran 的 DATA 语句开头部分
                line = "     {} (/{},".format(cont_num, line_part)
            # 处理最后一块（最后一个参数块的最后一行）
            elif i == rows - 1 and chunk_idx == len(chunks) - 1:
                # 格式化为 Fortran 的 DATA 语句结束部分
                line = "     {} {}/)".format(cont_num, line_part)
            # 处理中间块
            else:
                # 格式化为 Fortran 的 DATA 语句中间部分，带续行符和逗号
                line = "     {} {},".format(cont_num, line_part)
            # 将生成的行添加到参数行列表中
            param_lines.append(line)
            # 更新续行符计数器，使其在 1-9 之间循环
            cont_num = cont_num % 9 + 1  # 续行符循环1-9

    # 将所有参数行组合成一个参数块字符串
    params_block = "\n".join(param_lines)
    # 生成 Fortran 数组声明语句，包括 RESHAPE 函数用于初始化数组
    array_declaration = (
        "      REAL*8, DIMENSION({0}, {1}) :: CreepCEpara_q0= RESHAPE(\n"
        "{2},\n"
        "     {3} (/{0}, {1}/))".format(columns, rows, params_block, cont_num) #注意：Fortran 的 RESHAPE 是先列后行
    )

    # 读取 Fortran 模板文件的内容
    with open(template_path, 'r') as f:
        template_content = f.read()
    # 替换模板中的 `{{CreepCE_X}}` 占位符为实际的蠕变模型名称，例如 "CreepRCC_q0"
    template_content = template_content.replace("{{CreepCE_X}}", "Creep{}".format(model))

    # 以下是被注释掉的材料类型替换逻辑，目前不启用
    # if material in ["225Cr-1Mo", "9Cr-1Mo-V"]:
    #     MTYPE = 3
    # elif material == "800H":
    #     MTYPE = 2
    # else:
    #     MTYPE = 1
    # template_content = template_content.replace("{{MTYPE}}", str(MTYPE))

    # 使用正则表达式找到 `!CREEP_PARAMS_START` 和 `!CREEP_PARAMS_END` 之间的内容
    # `flags=re.DOTALL` 使得 `.` 匹配包括换行符在内的所有字符
    pattern = r'(!CREEP_PARAMS_START\s*).*?(!CREEP_PARAMS_END)'
    # 替换 `START` 和 `END` 之间的内容为生成的数组声明，不改变标记本身
    updated_content = re.sub(
        pattern,
        "\\1\n{}\n\\2".format(array_declaration),
        template_content,
        flags=re.DOTALL
    )


    # 使用正则表达式找到插入蠕变本构代码的地方，即 `!CreepCE_start` 和 `!CreepCE_end` 之间
    pattern1 = r'(!CreepCE_start\s*).*?(!CreepCE_end)'
    # 替换 `START` 和 `END` 之间的内容为实际的蠕变本构代码片段，不改变标记本身
    updated_content1 = re.sub(
        pattern1,
        "\\1\n{}\n\\2".format(updated_contentCE),
        updated_content,
        flags=re.DOTALL
    )
    # 返回最终更新后的 Fortran 代码内容
    return updated_content1
# 生成用户定义的塑性子程序 Fortran 代码
def generate_user_Plastic(prev_fortran_txt, Plastic_row):
    """
    prev_fortran_txt : 前面几步已经拼好的整段 Fortran 文本
    Plastic_row      : 形如 ['Plastic','User','ASME_225Cr1Mo','ver2021', []]
    """
    # 从 Plastic_row 中提取模型名称，例如 'ASME_225Cr1Mo'
    model = Plastic_row[2]        # 'ASME_225Cr1Mo'
    # 从 Plastic_row 中提取版本信息，例如 'ver2021' (这里暂时不细分版本)
    version = Plastic_row[3]      # 'ver2021' 这里只先不细分

    # 目前只支持 2.25Cr-1Mo 这一个模板，后面想扩也很容易
    if model == 'ASME_225Cr1Mo':
        # 构建模板文件的完整路径
        tpl_path = os.path.join(
            script_dir,
            r'fortranBase\2.25Cr-1Mo_Uhard_Creep.for'
        )
    # 如果模型不支持，则抛出 ValueError 异常
    else:
        raise ValueError('暂不支持的 UHARD 模型: {}'.format(model))

    # 读取模板文件的全部内容
    with open(tpl_path, 'r') as f:
        whole_tpl = f.read()

    # --- 用正则表达式把 UHARD 子程序抠出来 --------------------
    # 正则表达式匹配以 "SUBROUTINE UHARD" 开头，以 "END" 结尾的子程序块
    # `re.S` (DOTALL) 使 `.` 匹配所有字符包括换行符
    # `re.I` (IGNORECASE) 使匹配不区分大小写
    # `re.M` (MULTILINE) 使 `^` 和 `$` 匹配行的开头和结尾
    m = re.search(r'^\s*SUBROUTINE\s+UHARD.*?^\s*END\s*$',
                  whole_tpl, flags=re.S | re.I | re.M)
    # 如果没有找到 UHARD 子程序，则抛出 RuntimeError 异常
    if not m:
        raise RuntimeError('没在模板里找到 UHARD 子程序')

    # 提取匹配到的 UHARD 子程序文本，并去除首尾空白
    uhard_sub = m.group(0).strip()          # 先拿到原始文本
    # 调用 _fix_block_spacing 函数修正缩进/对齐 (此函数未在当前代码段中定义，假设其存在并完成此功能)
    uhard_sub = _fix_block_spacing(uhard_sub)   # ← 再做缩进/对齐修正

    # ── 3. 拼回主文件 ─────────────────────────────────────
    # 将前面生成的 Fortran 文本与 UHARD 子程序文本合并，并添加适当的换行符
    merged = prev_fortran_txt.rstrip() + '\n\n' + uhard_sub + '\n'
    # 返回合并后的 Fortran 代码
    return merged

# 选用哪个蠕变本构方程插入的辅助函数
def CreepNB_SH_q0():
    # 获取当前脚本所在目录的绝对路径（假设 script_dir 已定义）
    template_name = "fortranBase/Creep_User_defined/CreepNB_SH_q0.for"
    # 构建模板文件的完整路径
    template_path_Direct = os.path.join(script_dir, template_name)
    # 读取模板文件的内容
    with open(template_path_Direct, 'r') as f:
        CEtemplate_content = f.read()
    # 返回读取到的 Fortran 代码片段
    return CEtemplate_content
# 选用哪个蠕变本构方程插入的辅助函数
def CreepNB_TH_q0():
    # 获取当前脚本所在目录的绝对路径（假设 script_dir 已定义）
    template_name = "fortranBase/Creep_User_defined/CreepNB_TH_q0.for"
    # 构建模板文件的完整路径
    template_path_Direct = os.path.join(script_dir, template_name)
    # 读取模板文件的内容
    with open(template_path_Direct, 'r') as f:
        CEtemplate_content = f.read()
    # 返回读取到的 Fortran 代码片段
    return CEtemplate_content
# 选用哪个蠕变本构方程插入的辅助函数
def CreepNBPN_q0():
    # 获取当前脚本所在目录的绝对路径（假设 script_dir 已定义）
    template_name = "fortranBase/Creep_User_defined/CreepNBPN_q0.for"
    # 构建模板文件的完整路径
    template_path_Direct = os.path.join(script_dir, template_name)
    # 读取模板文件的内容
    with open(template_path_Direct, 'r') as f:
        CEtemplate_content = f.read()
    # 返回读取到的 Fortran 代码片段
    return CEtemplate_content
# 选用哪个蠕变本构方程插入的辅助函数
def CreepRCC_q0():
    # 获取当前脚本所在目录的绝对路径（假设 script_dir 已定义）
    template_name = "fortranBase/Creep_User_defined/CreepRCC_q0.for"
    # 构建模板文件的完整路径
    template_path_Direct = os.path.join(script_dir, template_name)
    # 读取模板文件的内容
    with open(template_path_Direct, 'r') as f:
        CEtemplate_content = f.read()
    # 返回读取到的 Fortran 代码片段
    return CEtemplate_content
# 处理 ASME_225Cr1Mo 蠕变模型的特殊函数
def Creep_ASME225Cr1Mo(tpl_txt):
    """tpl_txt 已是整段模板文本，返回替换后的文本"""
    # 读取外部 Fortran 文件内容，该文件包含 ASME_225Cr1Mo 模型的 CREEP 子程序
    ext = open(os.path.join(script_dir,
            r'fortranBase/2.25Cr-1Mo_Uhard_Creep.for')).read()

    # 导入正则表达式模块
    import re
    # 使用正则表达式从外部文件中提取 SUBROUTINE CREEP(...) 到 END 之间的代码块
    # `re.I` (IGNORECASE) 忽略大小写，`re.M` (MULTILINE) 使 ^ 和 $ 匹配行首行尾，`re.S` (DOTALL) 使 . 匹配所有字符包括换行符
    m = re.search(r'^\s*SUBROUTINE\s+CREEP\b.*?^\s*END\s*$',
              ext, flags=re.I | re.M | re.S)
    # 如果没有找到 SUBROUTINE CREEP，则抛出运行时错误
    if not m:
        raise RuntimeError('外部文件里找不到 SUBROUTINE CREEP')

    # 提取匹配到的 CREEP 子程序文本，并去除首尾空白
    creep_txt = m.group(0).strip()   # ① 先拿到原始子程序文本
    # 调用 _fix_block_spacing 函数修正缩进/对齐 (此函数未在当前代码段中定义，假设其存在并完成此功能)
    creep_txt = _fix_block_spacing(creep_txt)   # ② 再做缩进/对齐修正     # ← 同理
    # 使用正则表达式将模板文本中原有的 SUBROUTINE CREEP 块替换为从外部文件提取的块
    # `count=1` 确保只替换第一个匹配项
    tpl_txt = re.sub(r'^\s*SUBROUTINE\s+CREEP\b.*?^\s*END\s*$',
                     creep_txt, tpl_txt, count=1, flags=re.I|re.S|re.M)

    # 将模板中的 `{{CreepCE_X}}` 占位符替换为 "CREEP"
    tpl_txt = tpl_txt.replace('{{CreepCE_X}}', 'CREEP')

    # 使用正则表达式清空蠕变参数区，即 `!CREEP_PARAMS_START` 和 `!CREEP_PARAMS_END` 之间的内容
    # `\1` 和 `\2` 是对捕获组的引用，保留了起始和结束标记
    tpl_txt = re.sub(r'(!CREEP_PARAMS_START\s*).*?(!CREEP_PARAMS_END)',
                     r'\1\n\2', tpl_txt, flags=re.S)
    # 返回处理后的模板文本
    return tpl_txt



# 蠕变损伤分数计算函数
def generate_user_CreepRuptureLife(CreepFortran,user_Tr):
    # 从 user_Tr 列表中提取用户函数类型（例如 "user_CreepRuptureLife"）
    userFunc = user_Tr[0]    #user_CreepRuptureLife
    # 从 user_Tr 列表中提取方法类型（例如 "Larson_Miller" 或 "Direct"）
    method = user_Tr[1]      #Larson_Miller 或者 Direct
    # 从 user_Tr 列表中提取数据源（例如 "fitted_by_2023Test"）
    datasource = user_Tr[2]  #fitted_by_2023Test
    # 从 user_Tr 列表中提取具体数据
    shuju = user_Tr[-1]
    # 根据方法类型进行条件判断
    if method == "Direct":
        # 如果方法是 "Direct"，则调用 user_Cre 函数（此函数未在当前代码段中定义）
        updated_content1 = user_CreepRuptureLife_Direct(shuju)

# 这是一个函数调用或者条件分支的一部分，原始代码片段不完整

    # 如果蠕变断裂寿命计算方法是 "Larson_Miller"
    elif method == "Larson_Miller":
        # 调用 user_CreepRuptureLife_Larson_Miller 函数生成对应的 Fortran 代码片段
        updated_content1 = user_CreepRuptureLife_Larson_Miller(shuju)
    # 写入蠕变损伤计算使用的是哪种方法
    # 将 Fortran 模板中的占位符 {{CRDAMAGE}} 替换为实际的蠕变损伤方法名称
    template_content_all = CreepFortran.replace("{{CRDAMAGE}}", "CRDAMAGE_{}".format(method))
    # **使用正则表达式找到需要插入内容的地方**
    # 定义正则表达式模式，用于匹配 Fortran 模板中 `!Creep_Damage_start` 和 `!Creep_Damage_end` 之间的内容
    pattern = r'(!Creep_Damage_start\s*).*?(!Creep_Damage_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    # 使用 re.sub 进行替换，`\1` 和 `\2` 分别代表匹配到的起始和结束标记
    updated_content = re.sub(
        pattern, # 正则表达式模式
        "\\1{}\n\\2".format(updated_content1), # 替换字符串，将生成的 Fortran 代码插入到标记之间
        template_content_all, # 待处理的 Fortran 模板内容
        flags=re.DOTALL # 匹配模式，使 `.` 也能匹配换行符
    )
    # 返回更新后的 Fortran 代码内容
    return updated_content

# 定义函数 user_CreepRuptureLife_Direct，用于生成直接法蠕变断裂寿命的 Fortran 代码
def user_CreepRuptureLife_Direct(shuju):
    # 将输入数据赋值给 params_list
    params_list = shuju

    # 提取每个子列表的第一个元素作为温度列表
    TEMP_list = [sublist[0] for sublist in params_list]
    # 提取每个子列表从索引1开始到末尾的元素作为寿命-应力列表
    Sr = [sublist[1:] for sublist in params_list]
    # 获取 Sr 列表中第一个子列表的长度，作为目标长度
    target_length = len(Sr[0])
    # 遍历 Sr 列表，如果子列表长度小于目标长度，则用 '1' 填充至目标长度，否则保持不变
    Sr = [sublist + [1] * (target_length - len(sublist)) if len(sublist) < target_length else sublist for sublist in Sr]

    # 初始化一个空列表，用于存储温度参数的 Fortran 代码行
    TEMP_list_lines = []
    # 初始化续行符计数器，Fortran 中通常从1开始
    cont_num1 = 1
    # 将温度列表赋值给 entry1
    entry1 = TEMP_list
    # 将温度列表中的每个参数转换为 Fortran 的 D0 格式字符串（双精度浮点数）
    param_strs1 = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry1]
    # 将转换后的参数用逗号连接成字符串
    line_part1 = ", ".join(param_strs1)
    # 将参数字符串分块，每块最多包含5个元素，用于 Fortran 代码的格式化
    chunks1 = [param_strs1[j:j + 5] for j in range(1, len(param_strs1), 5)]
    # 遍历每个参数块
    for chunk_idx, chunk in enumerate(chunks1):
        # 将当前块的参数用逗号连接成字符串
        line_part1 = ", ".join(chunk)
        # 首行处理：如果是第一个块，则生成 DATA 语句的起始行
        if chunk_idx == 0:
            line1 = "      DATA TEMP_list/{},".format(line_part1)
            # 首行不需要续行符，所以设置为0
            cont_num1 = 0
        # 最后一块处理：如果是最后一个块，则生成 DATA 语句的结束行
        elif chunk_idx == len(chunks1) - 1:
            line1 = "     {} {}/".format(cont_num1, line_part1)
        # 中间块处理：生成 DATA 语句的中间行
        else:
            line1 = "     {} {},".format(cont_num1, line_part1)
        # 将生成的 Fortran 代码行添加到列表中
        TEMP_list_lines.append(line1)
        # 更新续行符计数器，使其在1-9之间循环
        cont_num1 = cont_num1 % 9 + 1  # 续行符循环1-9

    # 初始化一个空列表，用于存储寿命-应力参数的 Fortran 代码行
    Sr_lines = []
    # 获取 Sr 列表的行数
    rows = len(Sr)  # 获取数据行数
    # 续行符计数器 (1-9循环)，初始化为0
    cont_num = 0
    # 遍历 Sr 列表的每一行
    for i in range(rows):
        # 获取当前行的数据
        entry = Sr[i]
        # 将当前行中的每个参数转换为 Fortran 的 D0 格式字符串
        param_strs = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry]
        # 将转换后的参数用逗号连接成字符串
        line_part = ", ".join(param_strs)
        # 将参数字符串分块，每块最多包含5个元素
        chunks = [param_strs[j:j + 5] for j in range(0, len(param_strs), 5)]
        # 遍历当前行的每个参数块
        for chunk_idx, chunk in enumerate(chunks):
            # 将当前块的参数用逗号连接成字符串
            line_part = ", ".join(chunk)
            # 首行处理：如果是第一个数据行的第一个块，则生成 DATA 语句的起始行
            if i == 0 and chunk_idx == 0:
                line = "      DATA Sr/{},".format(line_part)
            # 最后一块处理：如果是最后一个数据行的最后一个块，则生成 DATA 语句的结束行
            elif i == rows-1 and chunk_idx == len(chunks) - 1:
                line = "     {} {}/".format(cont_num, line_part)
            # 中间块处理：生成 DATA 语句的中间行
            else:
                line = "     {} {},".format(cont_num, line_part)
            # 将生成的 Fortran 代码行添加到列表中
            Sr_lines.append(line)
            # 更新续行符计数器，使其在1-9之间循环
            cont_num = cont_num % 9 + 1  # 续行符循环1-9

    # 组合温度参数块的 Fortran 代码行，用换行符连接
    params_block1 = "\n".join(TEMP_list_lines)
    # 组合寿命-应力参数块的 Fortran 代码行，用换行符连接
    params_block = "\n".join(Sr_lines)
    # 生成 Fortran 数组声明的完整字符串
    array_declaration = ("{}\n" # 温度列表的 Fortran 代码
        "{}".format(params_block1,params_block)  # 寿命-应力列表的 Fortran 代码，注释说明是先列后行（Fortran 数组存储顺序）
    )

    # 获取当前脚本文件所在的目录的绝对路径
    # 假设 script_dir 变量已在其他地方定义
    template_name = "fortranBase/user_CreepRuptureLife/Direct.for"
    # 构建 Fortran 模板文件的完整路径
    template_path_Direct = os.path.join(script_dir, template_name)
    # 以只读模式打开模板文件
    with open(template_path_Direct, 'r') as f:
        # 读取模板文件的全部内容
        template_content = f.read()

    # 替换模板中的占位符 `{{TEMP_list}}` 为实际的温度列表长度减1（通常用于数组索引）
    template_content = template_content.replace("{{TEMP_list}}", "{}".format(len(TEMP_list)-1))
    # 替换模板中的占位符 `{{rows}}` 为 Sr 列表的列数（Fortran 数组的第二个维度）
    template_content = template_content.replace("{{rows}}", "{}".format(len(Sr[0])))
    # 替换模板中的占位符 `{{columns}}` 为 TEMP_list 的长度（Fortran 数组的第一个维度）
    template_content = template_content.replace("{{columns}}", "{}".format(len(TEMP_list)))

    # **使用正则表达式找到 `!Direct_start` 和 `!Direct_end` 之间的内容**
    # 定义正则表达式模式，用于匹配 Fortran 模板中 `!Direct_start` 和 `!Direct_end` 之间的内容
    pattern = r'(!Direct_start\s*).*?(!Direct_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    # 使用 re.sub 进行替换，将生成的数组声明代码插入到标记之间
    updated_content = re.sub(
        pattern, # 正则表达式模式
        "\\1\n{}\n\\2".format(array_declaration), # 替换字符串，`\1` 和 `\2` 保留标记
        template_content, # 待处理的 Fortran 模板内容
        flags=re.DOTALL # 匹配模式，使 `.` 也能匹配换行符
    )
    # 返回更新后的 Fortran 代码内容
    return updated_content

# 定义函数 user_CreepRuptureLife_Larson_Miller，用于生成 Larson-Miller 法蠕变断裂寿命的 Fortran 代码
def user_CreepRuptureLife_Larson_Miller(shuju):
    # 将输入数据赋值给 params_list
    params_list = shuju
    # 初始化一个空列表，用于存储参数的 Fortran 代码行
    param_lines = []
    # 获取 params_list 的行数
    rows = len(params_list)  # 行数
    # 获取 params_list 的列数，如果行数为0则列数为0
    columns = len(params_list[0]) if rows > 0 else 0  # 获取数据列数

    # 续行符计数器 (1-9循环)，初始化为1
    cont_num = 1
    # 遍历 params_list 的每一行
    for i in range(rows):
        # 获取当前行的数据
        entry = params_list[i]
        # 将当前行中的每个参数转换为 Fortran 的 D0 格式字符串
        param_strs = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry]
        # 将转换后的参数用逗号连接成字符串
        line_part = ", ".join(param_strs)
        # 将参数字符串分块，每块最多包含3个元素
        chunks = [param_strs[j:j + 3] for j in range(0, len(param_strs), 3)]
        # 遍历当前行的每个参数块
        for chunk_idx, chunk in enumerate(chunks):
            # 将当前块的参数用逗号连接成字符串
            line_part = ", ".join(chunk)
            # 首行处理：如果是第一个数据行的第一个块，则生成 DATA 语句的起始行
            if i == 0 and chunk_idx == 0:
                line = "     {} (/{},".format(cont_num, line_part)
                # 最后一块处理：如果是最后一个数据行的最后一个块，则生成 DATA 语句的结束行
            elif i == rows - 1 and chunk_idx == len(chunks) - 1:
                line = "     {} {}/)".format(cont_num, line_part)
            # 中间块处理：生成 DATA 语句的中间行
            else:
                line = "     {} {},".format(cont_num, line_part)

            # 将生成的 Fortran 代码行添加到列表中
            param_lines.append(line)
            # 更新续行符计数器，使其在1-9之间循环
            cont_num = cont_num % 9 + 1  # 续行符循环1-9

    # 组合所有参数块的 Fortran 代码行，用换行符连接
    params_block = "\n".join(param_lines)
    # 生成 Fortran 数组声明的完整字符串
    array_declaration = (
        "      LMpara=\n" # Larson-Miller 参数的 Fortran 变量声明
        "{}\n".format(params_block)  # 插入生成的参数块，注释说明是先列后行（Fortran 数组存储顺序）
    )

    # 获取当前脚本文件所在的目录的绝对路径
    # 假设 script_dir 变量已在其他地方定义
    template_name = "fortranBase/user_CreepRuptureLife/Larson_Miller.for"
    # 构建 Fortran 模板文件的完整路径
    template_path_Direct = os.path.join(script_dir, template_name)
    # 以只读模式打开模板文件
    with open(template_path_Direct, 'r') as f:
        # 读取模板文件的全部内容
        template_content = f.read()

    # **使用正则表达式找到需要插入的地方**
    # 定义正则表达式模式，用于匹配 Fortran 模板中 `!larson_miller_start` 和 `!larson_miller_end` 之间的内容
    pattern = r'(!larson_miller_start\s*).*?(!larson_miller_end)'

    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    # 使用 re.sub 进行替换，将生成的数组声明代码插入到标记之间
    updated_content = re.sub(
        pattern, # 正则表达式模式
        "\\1\n{}\n\\2".format(array_declaration), # 替换字符串，`\1` 和 `\2` 保留标记
        template_content, # 待处理的 Fortran 模板内容
        flags=re.DOTALL # 匹配模式，使 `.` 也能匹配换行符
    )
    # 返回更新后的 Fortran 代码内容
    return updated_content


# 疲劳损伤分数计算
# 定义函数 generate_user_FatigueLife，用于生成疲劳寿命计算的 Fortran 代码
def generate_user_FatigueLife(user_TrFortran,user_Nd):
    # 声明 updated_content1 为全局变量，以便在函数外部访问
    global updated_content1
    # 从 user_Nd 中提取用户函数名称，例如 user_FatigueLife
    userFunc = user_Nd[0]  # user_FatigueLife
    # 从 user_Nd 中提取疲劳寿命计算方法，例如 Direct, Langer, Manson_Coffin
    method = user_Nd[1]  # Direct or Langer or Manson_Coffin
    # 从 user_Nd 中提取数据来源，例如 ASME 或 fittedASME
    datasource = user_Nd[2]  # ASME or fittedASME
    # 从 user_Nd 中提取实际数据
    shuju = user_Nd[-1]
    # 根据选择的方法调用相应的函数生成 Fortran 代码片段
    if method == "Direct":
        # 如果是直接法，调用 user_FatigueLife_Direct
        updated_content1 = user_FatigueLife_Direct(shuju)
    elif method == "Langer":
        # 如果是 Langer 法，调用 user_FatigueLife_Langer
        updated_content1 = user_FatigueLife_Langer(shuju)
    elif method == "Manson_Coffin":
        # 如果是 Manson-Coffin 法，目前设置为1（ 表示待实现或占位符）
        updated_content1 = 1

    # 在主模板中写入疲劳寿命确定使用的是哪种方法
    # 将主 Fortran 模板中的占位符 {{user_FatigueLife_method}} 替换为实际的方法名称
    template_content_all = user_TrFortran.replace("{{user_FatigueLife_method}}", "{}".format(method))
    # 将生成的子模版插入主模板中
    # **使用正则表达式找到 `!Fatigue_life_model_start` 和 `!Fatigue_life_model_end` 之间的内容**
    # 定义正则表达式模式，用于匹配 Fortran 模板中 `!Fatigue_life_model_start` 和 `!Fatigue_life_model_end` 之间的内容
    pattern = r'(!Fatigue_life_model_start\s*).*?(!Fatigue_life_model_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    # 使用 re.sub 进行替换，将生成的疲劳寿命模型代码插入到标记之间
    updated_content = re.sub(
        pattern, # 正则表达式模式
        "\\1{}\n\\2".format(updated_content1), # 替换字符串，`\1` 和 `\2` 保留标记
        template_content_all, # 待处理的 Fortran 模板内容
        flags=re.DOTALL # 匹配模式，使 `.` 也能匹配换行符
    )
    # 返回更新后的 Fortran 代码内容
    return updated_content

# 定义函数 user_FatigueLife_Direct，用于生成直接法疲劳寿命的 Fortran 代码
def user_FatigueLife_Direct(shuju):
    # 将输入数据赋值给 params_list
    params_list = shuju
    # 初始化一个空列表，用于存储参数的 Fortran 代码行
    param_lines = []
    # 获取 params_list 的行数
    rows = len(params_list)  # 行数
    # 获取 params_list 的列数，如果行数为0则列数为0
    columns = len(params_list[0]) if rows > 0 else 0  # 获取数据列数

    # 续行符计数器 (1-9循环)，初始化为1
    cont_num = 1
    # 遍历 params_list 的每一行
    for i in range(rows):
        # 获取当前行的数据
        entry = params_list[i]
        # 将当前行中的每个参数转换为 Fortran 的 D0 格式字符串
        param_strs = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry]
        # 将转换后的参数用逗号连接成字符串
        line_part = ", ".join(param_strs)
        # 将参数字符串分块，每块最多包含5个元素
        chunks = [param_strs[j:j + 5] for j in range(0, len(param_strs), 5)]
        # 遍历当前行的每个参数块
        for chunk_idx, chunk in enumerate(chunks):
            # 将当前块的参数用逗号连接成字符串
            line_part = ", ".join(chunk)
            # 首行处理：如果是第一个数据行的第一个块，则生成 DATA 语句的起始行
            if i == 0 and chunk_idx == 0:
                line = "     {} (/{},".format(cont_num, line_part)
                # 最后一块处理：如果是最后一个数据行的最后一个块，则生成 DATA 语句的结束行
            elif i == rows - 1 and chunk_idx == len(chunks) - 1:
                line = "     {} {}/)".format(cont_num, line_part)
            # 中间块处理：生成 DATA 语句的中间行
            else:
                line = "     {} {},".format(cont_num, line_part)

            # 将生成的 Fortran 代码行添加到列表中
            param_lines.append(line)
            # 更新续行符计数器，使其在1-9之间循环
            cont_num = cont_num % 9 + 1  # 续行符循环1-9

    # 组合所有参数块的 Fortran 代码行，用换行符连接
    params_block = "\n".join(param_lines)
    # 生成 Fortran 数组声明的完整字符串，使用 RESHAPE 函数
    array_declaration = (
        "      REAL*8, DIMENSION({0}, {1}) :: SN= RESHAPE(\n" # 声明一个双精度实数二维数组 SN
        "{2},\n" # 数组数据
        "     {3} (/{0}, {1}/))".format(columns, rows, params_block, cont_num + 1)  # RESHAPE 的参数：目标形状 (columns, rows)
    )

    # 获取当前脚本文件所在的目录的绝对路径
    # 假设 script_dir 变量已在其他地方定义
    template_name = "fortranBase/user_Fatigue_Life/DirectModel.for"
    # 构建 Fortran 模板文件的完整路径
    template_path_Direct = os.path.join(script_dir, template_name)
    # 以只读模式打开模板文件
    with open(template_path_Direct, 'r') as f:
        # 读取模板文件的全部内容
        template_content = f.read()

    # 替换模板中的占位符 `{{strain_range_num}}` 为实际的应变范围或寿命的维数（行数减1）
    template_content = template_content.replace("{{strain_range_num}}", "{}".format(rows-1))
    # 替换模板中的占位符 `{{temps_num}}` 为实际的温度维数（列数减1）
    template_content = template_content.replace("{{temps_num}}", "{}".format(columns-1))

    # **使用正则表达式找到 `!CREEP_PARAMS_START` 和 `!CREEP_PARAMS_END` 之间的内容**
    # 定义正则表达式模式，用于匹配 Fortran 模板中 `!Fatigue_life_Directmodel_start` 和 `!Fatigue_life_Directmodel_end` 之间的内容
    pattern = r'(!Fatigue_life_Directmodel_start\s*).*?(!Fatigue_life_Directmodel_end)'

    # 替换 `START` 和 `END` 之间的内容，不改变标记本身
    # 使用 re.sub 进行替换，将生成的数组声明代码插入到标记之间
    updated_content = re.sub(
        pattern, # 正则表达式模式
        "\\1{}\n\\2".format(array_declaration), # 替换字符串，`\1` 和 `\2` 保留标记
        template_content, # 待处理的 Fortran 模板内容
        flags=re.DOTALL # 匹配模式，使 `.` 也能匹配换行符
    )
    # 返回更新后的 Fortran 代码内容
    return updated_content

# 定义函数 user_FatigueLife_Langer，用于生成 Langer 法疲劳寿命的 Fortran 代码
def user_FatigueLife_Langer(shuju):
    # 将输入数据赋值给 params_list
    params_list = shuju
    # 初始化一个空列表，用于存储参数的 Fortran 代码行
    param_lines = []
    # 获取 params_list 的行数
    rows = len(params_list)  # 行数
    # 获取 params_list 的列数，如果行数为0则列数为0
    columns = len(params_list[0]) if rows > 0 else 0  # 获取数据列数
    # 续行符计数器 (1-9循环)，初始化为1
    cont_num = 1
    # 遍历 params_list 的每一行
    for i in range(rows):
        # 获取当前行的数据
        entry = params_list[i]
        # 将当前行中的每个参数转换为 Fortran 的 D0 格式字符串
        param_strs = ["{}D0".format(p) if isinstance(p, (int, float)) else str(p) for p in entry]
        # 将转换后的参数用逗号连接成字符串
        line_part = ", ".join(param_strs)
        # 将参数字符串分块，每块最多包含3个元素
        chunks = [param_strs[j:j + 3] for j in range(0, len(param_strs), 3)]
        # 遍历当前行的每个参数块
        for chunk_idx, chunk in enumerate(chunks):
            # 将当前块的参数用逗号连接成字符串
            line_part = ", ".join(chunk)
            # 首行处理：如果是第一个数据行的第一个块，则生成 DATA 语句的起始行
            if i == 0 and chunk_idx == 0:
                line = "     {} (/{},".format(cont_num, line_part)
                # 最后一块处理：如果是最后一个数据行的最后一个块，则生成 DATA 语句的结束行
            elif i == rows - 1 and chunk_idx == len(chunks) - 1:
                line = "     {} {}/)".format(cont_num, line_part)
            # 中间块处理：生成 DATA 语句的中间行
            else:
                line = "     {} {},".format(cont_num, line_part)

            # 将生成的 Fortran 代码行添加到列表中
            param_lines.append(line)
            # 更新续行符计数器，使其在1-9之间循环
            cont_num = cont_num % 9 + 1  # 续行符循环1-9

    # 组合所有参数块的 Fortran 代码行，用换行符连接
    params_block = "\n".join(param_lines)
    # 生成 Fortran 数组声明的字符串。
    # 这个声明将用于在 Fortran 代码中定义一个双精度实数数组 Lpara。
    array_declaration = (
        # Fortran 数组声明的模板字符串。
        # DIMENSION({0}, {1}) 定义了数组的维度，其中 {0} 是列数，{1} 是行数。
        # RESHAPE 函数用于将一维数据重塑为指定维度的数组。
        "      REAL*8, DIMENSION({0}, {1}) :: Lpara= RESHAPE(\n"
        # {2} 将是实际的参数数据块。
        "{2},\n"
        # {3} 是 Fortran 内部的连续数，用于 RESHAPE 函数。
        "     {3} (/{0}, {1}/))".format(columns, rows, params_block, cont_num)  # 先列后行
    )

    # 获取当前 Python 脚本文件所在的目录的绝对路径。
    # 这确保了无论脚本从何处运行，都能正确找到模板文件。
    template_name = "fortranBase/user_Fatigue_Life/LangerModel.for"
    # 使用 os.path.join 构建模板文件的完整路径。
    # 这种方法可以确保路径在不同操作系统上的兼容性。
    template_path_Langer = os.path.join(script_dir, template_name)
    # 以只读模式打开 Fortran 模板文件。
    with open(template_path_Langer, 'r') as f:
        # 读取模板文件的全部内容。
        template_content = f.read()

    # 替换模板中占位符 `{{para}}` 为实际的列数减一。
    # 这通常代表 Fortran 数组的某个维度或相关参数。
    template_content = template_content.replace("{{para}}", "{}".format(columns - 1))
    # 替换模板中占位符 `{{temps}}` 为实际的行数。
    # 这通常代表 Fortran 数组的另一个维度或相关参数。
    template_content = template_content.replace("{{temps}}", "{}".format(rows))

    # **使用正则表达式找到 Fortran 模板中特定标记之间的内容。**
    # `!langer_start` 和 `!langer_end` 是 Fortran 注释，用于标识需要插入代码的区域。
    pattern = r'(!langer_start\s*).*?(!langer_end)'

    # 使用正则表达式替换 `START` 和 `END` 标记之间的内容。
    # `\1` 和 `\2` 分别代表正则表达式捕获组中的 `!langer_start` 和 `!langer_end`。
    # 这样可以确保标记本身不会被替换掉，只替换它们之间的内容。
    updated_content = re.sub(
        pattern,
        # 在两个标记之间插入生成的 Fortran 数组声明。
        "\\1{}\n\\2".format(array_declaration),
        template_content,
        flags=re.DOTALL # re.DOTALL 标志使得 `.` 可以匹配包括换行符在内的所有字符。
    )
    # 返回更新后的 Fortran 代码内容。
    return updated_content


# 定义一个函数，用于生成用户自定义的蠕变-疲劳损伤包络线 Fortran 代码。
# 蠕变-疲劳损伤包络线
def generate_user_CreepFatigueInteractionCriterion(user_NdFortran,user_CFInter):
    # 从输入数据 user_CFInter 中提取蠕变损伤点。
    pointCreep = user_CFInter[-1][0][0]
    # 从输入数据 user_CFInter 中提取疲劳损伤点。
    pointFatigue = user_CFInter[-1][0][1]
    # 调用 calculate_lines 函数计算包络线的斜率和截距。
    k1, b1, k2, b2, k3 = calculate_lines(pointCreep, pointFatigue)
    # 获取当前脚本文件所在目录的绝对路径。
    template_name = "fortranBase/user_CreepFatigueInteractionCriterion/is_above.for"
    # 构建模板文件的完整路径。
    template_path_Direct = os.path.join(script_dir, template_name)
    # 以只读模式打开 Fortran 模板文件。
    with open(template_path_Direct, 'r') as f:
        # 读取模板文件的全部内容。
        template_content = f.read()
    # 替换模板中的斜率 k1 占位符。
    # 替换k和b
    template_content = template_content.replace("{{k1}}", "{}".format(k1))
    # 替换模板中的截距 b1 占位符。
    template_content = template_content.replace("{{b1}}", "{}".format(b1))
    # 替换模板中的斜率 k2 占位符。
    template_content = template_content.replace("{{k2}}", "{}".format(k2))
    # 替换模板中的截距 b2 占位符。
    template_content = template_content.replace("{{b2}}", "{}".format(b2))
    # 替换模板中的斜率 k0 (k3) 占位符，并存储为 updated_content1。
    updated_content1 = template_content.replace("{{k0}}", "{}".format(k3))

    # 读入已连续修改的主模板。
    # user_NdFortran 包含了之前 已经修改过的 Fortran 代码内容。
    template_content_all = user_NdFortran

    # 将生成的子模版插入主模板中。
    # **使用正则表达式找到 `!Creep_Fatigue_Enevlope_start` 和 `!Fatigue_life_model_end` 之间的内容**
    # 定义正则表达式模式，用于匹配 Fortran 模板中的特定注释标记。
    pattern = r'(!Creep_Fatigue_Enevlope_start\s*).*?(!Creep_Fatigue_Enevlope_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身。
    # 将之前生成的 updated_content1 (包含计算出的 k, b 值) 插入到主模板的指定位置。
    updated_content = re.sub(
        pattern,
        "\\1{}\n\\2".format(updated_content1),
        template_content_all,
        flags=re.DOTALL # 确保正则表达式的 `.` 能匹配换行符。
    )
    # 返回包含蠕变-疲劳损伤包络线代码的更新后的 Fortran 内容。
    return updated_content

# 定义一个辅助函数，用于计算连接两个点和坐标轴点的直线的斜率和截距。
def calculate_lines(x0, y0):
    # 计算第一条线（到纵轴点 (0,1)）蠕变损伤线。
    # 这条线连接点 (x0, y0) 和 (0, 1)。
    try:
        # 计算斜率 k1。如果 x0 为 0，则斜率未定义或为无穷大。
        k1 = (y0 - 1.0) / x0 if x0 != 0 else None
        # 截距 b1 始终为 1.0，因为直线通过 (0, 1)。
        b1 = 1.0
    except ZeroDivisionError:
        # 捕获除以零的错误，将 k1 和 b1 设置为 None。
        k1, b1 = None, None
    # 计算第二条线（到横轴点 (1,0)）。
    # 这条线连接点 (x0, y0) 和 (1, 0)。
    try:
        # 计算斜率 k2。如果 (1.0 - x0) 为 0，则斜率未定义或为无穷大。
        k2 = (0.0 - y0) / (1.0 - x0) if (1.0 - x0) != 0 else None
        # 通过点 (1,0) 和斜率 k2 计算截距 b2。
        b2 = 0.0 - k2 * 1.0  # 通过点(1,0)计算截距
    except ZeroDivisionError:
        # 捕获除以零的错误，将 k2 和 b2 设置为 None。
        k2, b2 = None, None
    # 计算第三条线（通过原点 (0,0) 和 (x0, y0) 的斜率）。
    # 这通常用于表示一个比例关系。
    k3 = y0/x0
    # 返回计算出的所有斜率和截距。
    return k1, b1, k2, b2, k3

# 定义一个函数，用于生成用户自定义的等效应力模型和设计系数的 Fortran 代码。
# 等效应力模型和设计系数
def generate_user_RepresentativeStress(user_CFInterFortran,user_EquivS):
    # 从 user_EquivS 中提取所选的方法。
    method = user_EquivS[1]
    # 根据方法选择不同的等效应力模型。
    if method == "RCCmethod(Hayhurst)":
        # 如果是 RCC 方法，提取安全系数 SF。
        SF = user_EquivS[2][0][0]
        # 调用 RCC 函数生成相应的 Fortran 代码。
        updated_content1 = RCC(SF,user_CFInterFortran)
    else:
        # 如果是 ASME 方法，提取安全系数 SF、常数 C 和材料类型 MTYPE。
        SF = user_EquivS[2][0][0]
        C = user_EquivS[2][0][1]
        MTYPE = user_EquivS[2][0][2]
        # 调用 ASME 函数生成相应的 Fortran 代码。
        updated_content1 = ASME(SF,C,MTYPE,user_CFInterFortran)
    # 返回生成的 Fortran 代码。
    return updated_content1

# 定义 RCC 方法的 Fortran 代码生成函数。
def RCC(SF,user_CFInterFortran):
    # 获取当前脚本文件所在目录的绝对路径。
    template_name = "fortranBase/user_RepresentativeStress/RCC.for"
    # 构建模板文件的完整路径。
    template_path_Direct = os.path.join(script_dir, template_name)
    # 以只读模式打开 RCC 模板文件。
    with open(template_path_Direct, 'r') as f:
        # 读取模板文件的全部内容。
        template_content = f.read()
    # 读入已连续修改的主模板。
    template_content_all = user_CFInterFortran
    # 替换模板中的安全系数 SF 占位符。
    template_content_all = template_content_all.replace("{{SF}}", "{}".format(SF))
    # 替换模板中的等效应力计算函数名占位符为 "RCC_SQ"。
    template_content_all = template_content_all.replace("{{REP_SQ}}", "RCC_SQ")
    # 将生成的子模版插入主模板中。
    # **使用正则表达式找到 `!REP_SQ_start` 和 `!Creep_Damage_end` 之间的内容**
    # 定义正则表达式模式，用于匹配 Fortran 模板中的特定注释标记。
    pattern =r'(!REP_SQ_start\s*).*?(!REP_SQ_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身。
    updated_content = re.sub(
        pattern,
        "\\1{}\n\\2".format(template_content),
        template_content_all,
        flags=re.DOTALL # 确保正则表达式的 `.` 能匹配换行符。
    )
    # 返回包含 RCC 等效应力模型代码的更新后的 Fortran 内容。
    return updated_content

# 定义 ASME 方法的 Fortran 代码生成函数。
def ASME(SF,C,MTYPE,user_CFInterFortran):
    # 获取当前脚本文件所在目录的绝对路径。
    template_name = "fortranBase/user_RepresentativeStress/ASME.for"
    # 构建模板文件的完整路径。
    template_path_Direct = os.path.join(script_dir, template_name)
    # 以只读模式打开 ASME 模板文件。
    with open(template_path_Direct, 'r') as f:
        # 读取模板文件的全部内容。
        template_content = f.read()
    # 根据常数 C 的值调整模板内容。
    if C in (0, 0.16, 0.24):
        # 如果 C 在特定值范围内，替换材料类型 MTYPE 和 C 为 0.24。
        template_content = template_content.replace("{{MTYPE}}", "{}".format(MTYPE))
        template_content = template_content.replace("{{C}}", "{}".format(0.24))
    else:
        # 否则，替换材料类型 MTYPE 为 99，并使用原始的 C 值。
        template_content = template_content.replace("{{MTYPE}}", "{}".format(99))
        template_content = template_content.replace("{{C}}", "{}".format(C))
    # 读入已连续修改的主模板。
    template_content_all = user_CFInterFortran
    # 替换模板中的安全系数 SF 占位符。
    template_content_all = template_content_all.replace("{{SF}}", "{}".format(SF))
    # 替换模板中的等效应力计算函数名占位符为 "ASME_SQ"。
    template_content_all = template_content_all.replace("{{REP_SQ}}", "ASME_SQ")
    # 将生成的子模版插入主模板中。
    # **使用正则表达式找到 `!REP_SQ_start` 和 `!Creep_Damage_end` 之间的内容**
    # 定义正则表达式模式，用于匹配 Fortran 模板中的特定注释标记。
    pattern = r'(!REP_SQ_start\s*).*?(!REP_SQ_end)'
    # 替换 `START` 和 `END` 之间的内容，不改变标记本身。
    updated_content = re.sub(
        pattern,
        "\\1{}\n\\2".format(template_content),
        template_content_all,
        flags=re.DOTALL # 确保正则表达式的 `.` 能匹配换行符。
    )
    # 返回包含 ASME 等效应力模型代码的更新后的 Fortran 内容。
    return updated_content

# 当脚本作为主程序运行时执行的代码块。
if __name__ == '__main__':
    # 定义 JSON 数据库文件的路径。
    JSON_PATH = (r'D:\SIMULIA\EstProducts\2023\win_b64\code\python2.7'
                r'\lib\abaqus_plugins\STPM_test1034\MaterialData.json')

    # ===== ② 读取数据库，只要 2.25Cr1Mo 这一支 =====
    # 以只读模式打开 JSON 文件。
    with open(JSON_PATH, 'r') as fp:
        # 从 JSON 文件中加载所有材料数据库内容。
        all_mat_db = json.load(fp)

    # 定义要查找的材料键名。
    MAT_KEY = '2.25Cr1Mo'            # JSON 里外层键
    # 尝试从加载的数据库中获取指定材料的数据。
    try:
        mat_json = all_mat_db[MAT_KEY]
    except KeyError:
        # 如果材料不存在，则抛出运行时错误。
        raise RuntimeError('MaterialData.json 里没有 “2.25Cr1Mo” 这一条!')

    # ===== ③ 依照界面勾选, 只保留 √ 的那两条 =============
    # 定义用户勾选的子模型名称。
    SEL_MODEL = 'ASME_225Cr1Mo'      # 勾选的子模型
    # 定义用户勾选的版本。
    SEL_VER   = 'ver2021'

    # ---- 3.1  Creep → User_defined -----------------------
    # 获取或创建 'Creep' -> 'User_defined' 路径下的字典。
    creep_user = mat_json.setdefault('Creep', {}) \
                        .setdefault('User_defined', {})

    # 遍历 'Creep' -> 'User_defined' 下的所有子模型。
    # 把同级的 NB_TH_q0 / NB_SH_q0 … 都干掉，只留 ASME_225Cr1Mo
    for sub in list(creep_user.keys()):
        # 如果子模型不是用户选择的模型，则将其从字典中移除。
        if sub != SEL_MODEL:
            creep_user.pop(sub)

    # 确保用户选择的模型和版本路径存在，并将其值设置为一个空列表。
    creep_user.setdefault(SEL_MODEL, {}) \
            .setdefault(SEL_VER, [])       # 保证路径存在 (空列表即可)

    # ---- 3.2  Plastic → User -----------------------------
    # 获取或创建 'Plastic' -> 'User' 路径下的字典。
    plast_user = mat_json.setdefault('Plastic', {}) \
                        .setdefault('User', {})

    # 遍历 'Plastic' -> 'User' 下的所有子模型。
    for sub in list(plast_user.keys()):
        # 如果子模型不是用户选择的模型，则将其从字典中移除。
        if sub != SEL_MODEL:
            plast_user.pop(sub)

    # 确保用户选择的模型和版本路径存在，并将其值设置为一个空列表。
    plast_user.setdefault(SEL_MODEL, {}) \
            .setdefault(SEL_VER, [])

    # ===== ④ 调用你的主入口 ================================
    # 定义目标材料在 Abaqus 模型中的名称。
    aimMaterialName = 'M225Cr1Mo'   # Abaqus Model 里的材料名
    # 定义 UVARM 变量的数量。
    UVARMnum        = 33
    # 定义 SDV 变量的数量。
    SDVnum          = 0

    # 调用主材料导入函数，传入处理后的材料 JSON 数据、目标材料名、UVARM 和 SDV 数量。
    # 假设 pre_materialImport_main 是一个已定义的函数，用于处理材料数据并 生成 Fortran 代码。
    pre_materialImport_main(mat_json,
                            aimMaterialName,
                            UVARMnum,
                            SDVnum)