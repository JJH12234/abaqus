# -*- coding: utf-8 -*-
# 这是一个Python脚本，指定了文件编码为UTF-8。
"""
Created on Tue Mar  4 11:36:31 2025

@author: mrvoid
"""
# 导入xlrd库，用于读取Excel文件。
import xlrd
# 从abaqus模块导入所有内容，这是Abaqus脚本编程的基础。
from abaqus import *
# 从abaqusConstants模块导入所有常量，例如YES, NO, CANCEL等。
from abaqusConstants import *

# 定义一个函数，用于获取当前Abaqus会话中正在显示的模型。
def get_current_model():
    # 函数的文档字符串，简要说明其功能。
    """获取当前视口关联的模型"""
    # 初始化一个标志变量为None。
    flag=None
    # 获取当前活跃视口的名称。
    viewport = session.currentViewportName
    # 从会话状态中获取当前视口所显示的模型名称。
    modelname=session.sessionState[viewport]['modelName']
    # 检查模型名称是否包含'Model-0'，这是Abaqus默认创建的模型名称。
    if 'Model-0' in modelname:
        # 如果是'Model-0'，则弹出一个警告对话框，提示用户不建议直接编辑此模型。
        # 用户可以选择继续、复制新模型或取消。
        flag=getWarningReply(
            u'警告： 不推荐用户自行编辑Model-0！\n YES以强制编辑; No建立副本并编辑;'.encode('GB18030'), (YES,NO,CANCEL))
    # 如果用户选择“No”（不继续编辑，复制新模型）。
    if flag==NO:
        # 为新模型生成一个建议的名称，将'Model-0'替换为'NewModel'。
        newname=modelname.replace('Model-0','NewModel')
        # 检查新名称是否已存在于当前模型的键中，以避免命名冲突。
        if newname in mdb.models.keys():
            # 如果已存在，则在新名称后添加'-Copy'。
            newname=newname+'-Copy'
        # 创建一个新模型，通过复制现有模型来创建。
        mdb.Model(name=newname,
                  objectToCopy=mdb.models[modelname])
        # 获取新模型的根装配体。
        a = mdb.models[newname].rootAssembly
        # 将当前视口显示的对象设置为新模型的根装配体。
        session.viewports[viewport].setValues(displayedObject=a)
        # 返回新创建的模型对象。
        return mdb.models[newname]
    # 如果用户选择“CANCEL”（取消操作）。
    elif flag==CANCEL:
        # 抛出一个异常，指示用户取消了操作。
        raise Exception('User Cancels when edit {mm}'.format(mm=modelname))
    # 如果用户选择“YES”或模型名称不包含'Model-0'，则直接返回当前模型。
    return mdb.models[modelname]

def pre_paraModeling(ParaList):
    # 获取当前模型对象。
    m = get_current_model()

    # 按类型拆分
    [sketch_data, features_data] = process_parameters(ParaList)

    # —— 按 part & feature 分组草图参数 —— #
    p_f_s = {}
    for row in sketch_data:
        if not row or len(row) < 5:
            continue
        partname = row[3]            # 第四列：part
        featurename = row[4]         # 第五列：feature
        key = (partname, featurename)
        if key not in p_f_s:
            p_f_s[key] = []
        p_f_s[key].append(row)

    # —— 逐草图修改 —— #
    for key, rows in p_f_s.items():
        part_key = str(key[0]).strip()
        feat_key = str(key[1]).strip()

        # part 严格命中
        if part_key in m.parts:
            part = m.parts[part_key]

            # feature 先严格命中
            if feat_key in part.features.keys():
                f = part.features[feat_key]
            else:
                # 最小兜底：忽略大小写 + 去首尾空格 做一次匹配
                hit = None
                low_target = feat_key.lower()
                for k in part.features.keys():
                    if k.strip().lower() == low_target:
                        hit = k
                        break
                if hit is None:
                    print(u"{p} has no {f} feature. Available: {keys}"
                          .format(p=part_key, f=feat_key,
                                  keys=u', '.join(part.features.keys())))
                    continue
                f = part.features[hit]

            # 切视口（可选）
            try:
                viewport = session.currentViewportName
                session.viewports[viewport].setValues(displayedObject=part)
            except Exception:
                pass

            # 修改草图参数
            paraModeling_sketch(m, f, rows)

    # —— 逐特征修改 —— #
    for row in features_data:
        if not row or len(row) < 5:
            continue
        part_key = str(row[3]).strip()
        feat_key = str(row[4]).strip()

        if part_key in m.parts:
            part = m.parts[part_key]
            try:
                # 先严格命中
                if feat_key in part.features.keys():
                    f = part.features[feat_key]
                else:
                    # 最小兜底：忽略大小写 + 去首尾空格
                    hit = None
                    low_target = feat_key.lower()
                    for k in part.features.keys():
                        if k.strip().lower() == low_target:
                            hit = k
                            break
                    if hit is None:
                        print(u"{p} has no {f} feature. Available: {keys}"
                              .format(p=part_key, f=feat_key,
                                      keys=u', '.join(part.features.keys())))
                        continue
                    f = part.features[hit]

                # 真正设置特征参数
                paraModeling_features(f, row)

            except KeyError:
                # ✅ 用当前行的 part_key / feat_key 打印，避免“串变量”
                print(u"{p} has no {f} feature".format(p=part_key, f=feat_key))
                pass
        else:
            print(u"[WARN] Part '{0}' not found in model.".format(part_key))

    # —— 重生成 —— #
    paraModeling_regen(m)
    mesh_regen(m)
    ass_regen(m)
# 定义一个函数，用于参数化修改草图。
def paraModeling_sketch(m,feature,paralist):
    # 获取特征的原始草图对象。
    s0 = feature.sketch
    # 复制原始草图，创建一个名为'__edit__'的新草图用于编辑。
    # 复制原图为edit
    m.ConstrainedSketch(name='__edit__', objectToCopy=s0)
    # 获取新创建的草图对象。
    s1 = m.sketches['__edit__']
    # 获取草图的几何、顶点、尺寸和约束对象，虽然这里没有直接使用这些变量。
    g, v, d, c = s1.geometry, s1.vertices, s1.dimensions, s1.constraints
    # 设置草图为主要编辑对象，并叠加显示，以便进行修改。
    s1.setPrimaryObject(option=SUPERIMPOSE)#编辑草图__edit__
    # 遍历参数列表中的每一行。
    for row in paralist:
        # 获取参数名称。
        paraname=str(row[0])
        # 获取参数表达式。
        expression=str(row[1])
        # 检查参数名称是否已存在于草图的参数中。
        if paraname in s1.parameters:
            # 如果存在，则更新其表达式。
            s1.parameters[paraname].setValues(expression=expression)
        # 如果参数不存在。
        else:
            # 创建一个新的草图参数。
            s1.Parameter(name=paraname, expression=expression, previousParameter=s1.parameters.keys()[-1])
            # 打印提示信息，表示已添加新参数。
            print("sketche has no {para}! Now added!".format(para=paraname))
    # 取消设置草图为主要编辑对象，结束编辑。
    s1.unsetPrimaryObject()#结束编辑草图
    # 将特征的草图设置为修改后的草图。
    feature.setValues(sketch=s1)
    # 删除临时编辑草图，清理模型。
    del m.sketches['__edit__']

# 定义一个函数，用于参数化修改特征。
def paraModeling_features(feature,data):
    # 获取数据中的第二个元素作为特征的值。
    value=data[1]
    # 设置特征的值。
    feature.setValues(value)

# 定义一个函数，用于处理和分离参数数据。
def process_parameters(data):
    # 初始化两个空列表，分别用于存储草图数据和特征数据。
    # 分离sketch和features数据
    sketch_data = []
    features_data = []
    # 根据数据的第四列（部件名称）进行排序。
    sorted_data = sorted(data, key=lambda x: x[3])
    # 遍历排序后的每一行数据。
    for row in sorted_data:  # 跳过标题行 (此注释可能不准确，实际是遍历所有行)
        # 检查第三列（参数类型）是否以'sketch'开头。
        if row[2].split('.')[0]=='sketch': #row[2]=='sketch.parameters'
            # 如果是草图参数，则添加到sketch_data列表。
            sketch_data.append(row)
        # 否则，认为是特征参数。
        else:
            # 添加到features_data列表。
            features_data.append(row)
    # 返回分离后的草图数据和特征数据。
    return sketch_data,features_data

# 定义一个函数，用于重新生成模型中的所有部件和装配体。
def paraModeling_regen(m):
    # 遍历模型中所有部件的名称。
    for partname in m.parts.keys():
        try:
            # 尝试重新生成当前部件。
            m.parts[partname].regenerate()
        # 捕获任何异常，例如部件无法重新生成。
        except:
            # 打印部件重新生成失败的错误信息。
            print("{part} regen fails!".format(part=partname))
            # 继续执行，忽略当前错误。
            pass
    # 重新生成模型的根装配体。
    m.rootAssembly.regenerate()

# 定义一个函数，用于重新生成模型中所有部件的网格。
def mesh_regen(m):
    # 提示：需要注意修改网格类型（传热/力学），这里只是生成网格。
    #记得修改网格类型：传热/力学
    # 遍历模型中所有部件的名称。
    for part in m.parts.keys():
        # 为当前部件生成网格。
        m.parts[part].generateMesh()
    # 占位符，表示后续可能需要添加网格检测功能。
    pass#后续需要网格检测

# 定义一个函数，用于重新生成装配体。
def ass_regen(m):
    # 重新生成模型的根装配体。
    m.rootAssembly.regenerate()

# 定义一个函数，用于从Excel文件读取数据并返回元组。
def read_excel_to_tuple(xls_path,xls_sheetname):
    # 函数的文档字符串，说明其功能和适用环境。
    """示例：用 xlrd 读 Excel（Python2.7 环境可用）"""
    # 打开Excel工作簿。
    book = xlrd.open_workbook(xls_path)
    # 根据工作表名称获取工作表对象。
    sheet = book.sheet_by_name(xls_sheetname)
    # 遍历工作表的每一行，将每行数据转换为元组，最终返回一个包含所有行元组的元组。
    return tuple(tuple(sheet.row_values(i)) for i in range(sheet.nrows))

# 定义一个主函数，供GUI直接调用，只接收文件路径和工作表名称。
def pre_paraModeling_main(xls_path,xls_sheetname):
    # 函数的文档字符串，说明其用途。
    """供 GUI 直接调用：只传路径"""
    # 调用read_excel_to_tuple函数读取Excel数据。
    data = read_excel_to_tuple(xls_path,xls_sheetname)
    # 调用pre_paraModeling函数进行参数化建模。
    pre_paraModeling(data)
# 脚本的入口点，当脚本直接运行时执行。
# 调用函数
if __name__ == '__main__':
    # 定义一个示例Excel文件路径。
    demo = u'd:/SIMULIA/EstProducts/2023/win_b64/code/python2.7/lib/abaqus_plugins/STPM_test1034/ParaModelingData.xls'
    # 这是一个被注释掉的示例路径，可用于测试。
    # data=read_excel_to_tuple(u'C:\\Users\\mrvoid\\abaqus_plugins\\STPM_test1035\\ParaModelingData.xls')
    # 调用主函数，传入示例路径和工作表名称'XGB'。
    pre_paraModeling_main(demo,'XGB')