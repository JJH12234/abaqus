# -*- coding: utf-8 -*-
"""
Created on Tue Mar  4 11:36:31 2025

@author: mrvoid
"""
import xlrd
from abaqus import *
from abaqusConstants import *
def get_current_model():
    """获取当前视口关联的模型"""
    flag=None
    viewport = session.currentViewportName
    modelname=session.sessionState[viewport]['modelName']
    if 'Model-0' in modelname:
        flag=getWarningReply(
            'WARRNING: Edit Model-0 is not recommanded for user!\n YES-continue; No-copyNew;', (YES,NO,CANCEL))
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
def pre_paraModeling(ParaList):
    m=get_current_model()
    #按类型拆分
    [sketch_data,features_data]=process_parameters(ParaList)
    #按part features拆分草图参数
    p_f_s = {}
    for row in sketch_data:
        partname = row[3]  # 第四列是 part
        featurename = row[4]  # 第五列是 feature
        key = (partname, featurename)  # 使用 (part, feature) 作为键
        if key not in p_f_s:
            p_f_s[key] = []
        p_f_s[key].append(row)
    #按草图统一调起sketch修改函数
    for key, rows in p_f_s.items():
        # print(key)
        if str(key[0]) in m.parts:
            f=m.parts[str(key[0])].features[str(key[1])]
            viewport = session.currentViewportName
            session.viewports[viewport].setValues(displayedObject=m.parts[str(key[0])])
            paraModeling_sketch(m,f,rows)
    #调起其他修改函数
    for row in features_data:
        if str(key[0]) in m.parts:
            try:
                f=m.parts[str(row[3])].features[str(row[4])]
                paraModeling_features(f,row)
            except KeyError:
                print("{p} has no {f} feature".format(p=partname,f=featurename))
                pass
    #模型重生成
    paraModeling_regen(m)
    #网格重生成
    mesh_regen(m)
    ass_regen(m)

def paraModeling_sketch(m,feature,paralist):
    #复制原图为edit
    s0 = feature.sketch
    m.ConstrainedSketch(name='__edit__', objectToCopy=s0)
    s1 = m.sketches['__edit__']
    g, v, d, c = s1.geometry, s1.vertices, s1.dimensions, s1.constraints
    s1.setPrimaryObject(option=SUPERIMPOSE)#编辑草图__edit__
    for row in paralist:
        paraname=str(row[0])
        expression=str(row[1])
        if paraname in s1.parameters:
            s1.parameters[paraname].setValues(expression=expression)    
        else:
            s1.Parameter(name=paraname, expression=expression, previousParameter=s1.parameters.keys()[-1])
            print("sketche has no {para}! Now added!".format(para=paraname))
    s1.unsetPrimaryObject()#结束编辑草图
    feature.setValues(sketch=s1)
    del m.sketches['__edit__']

def paraModeling_features(feature,data):
    value=data[1]
    feature.setValues(value)

def process_parameters(data):
    # 分离sketch和features数据
    sketch_data = []
    features_data = []
    sorted_data = sorted(data, key=lambda x: x[3])
    for row in sorted_data:  # 跳过标题行
        if row[2].split('.')[0]=='sketch': #row[2]=='sketch.parameters'
            sketch_data.append(row)
        else:
            features_data.append(row)
    return sketch_data,features_data

def paraModeling_regen(m):
    for partname in m.parts.keys():
        try:
            m.parts[partname].regenerate()
        except:
            print("{part} regen fails!".format(part=partname))
            pass
    m.rootAssembly.regenerate()

def mesh_regen(m):
    #记得修改网格类型：传热/力学
    for part in m.parts.keys():
        m.parts[part].generateMesh()
    pass#后续需要网格检测

def ass_regen(m):
    m.rootAssembly.regenerate()
    

def read_excel_to_tuple(xls_path,xls_sheetname):
    """示例：用 xlrd 读 Excel（Python2.7 环境可用）"""
    book = xlrd.open_workbook(xls_path)
    sheet = book.sheet_by_name(xls_sheetname)
    return tuple(tuple(sheet.row_values(i)) for i in range(sheet.nrows))

def pre_paraModeling_main(xls_path,xls_sheetname):
    """供 GUI 直接调用：只传路径"""
    data = read_excel_to_tuple(xls_path,xls_sheetname)
    pre_paraModeling(data)
# 调用函数
if __name__ == '__main__':
    demo = u'd:/SIMULIA/EstProducts/2023/win_b64/code/python2.7/lib/abaqus_plugins/STPM_test1034/ParaModelingData.xls'
    # data=read_excel_to_tuple(u'C:\\Users\\mrvoid\\abaqus_plugins\\STPM_test1035\\ParaModelingData.xls')
    pre_paraModeling_main(demo,'XGB')
    
