# -*- coding: utf-8 -*-
import xlrd
from abaqus import *
from abaqusConstants import *
import re
from collections import Counter, defaultdict
WARRNINGflag=None
def get_current_model():
    global WARRNINGflag
    """获取当前视口关联的模型"""
    flag=None
    viewport = session.currentViewportName
    modelname=session.sessionState[viewport]['modelName']
    if 'Model-0' in modelname and WARRNINGflag!=YES:
        flag=getWarningReply(
            'WARRNING: Edit Model-0 is not recommanded for user!\n YES-continue; No-copyNew;', (YES,NO,CANCEL))
        WARRNINGflag=flag
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

def data_proceed(data_tuple, data_name):
    # 处理你的数据和数据名称
    time_column = [t for t, _ in data_tuple]
    
    # 检查时间列是否升序
    if any(time_column[i] >= time_column[i + 1] for i in range(len(time_column) - 1)):
        # print("警告：时间列不是严格升序的！", data_name)
        return  # 如果时间列不是升序，跳过处理
    m=get_current_model()
    m.TabularAmplitude(name=data_name, timeSpan=STEP, 
        smooth=SOLVER_DEFAULT, data=data_tuple,)


def pre_ampTableChange(file_path,template='%OP%_%TT%'):
    # 打开Excel文件
    workbook = xlrd.open_workbook(file_path)
    ReTranslater = {} 
    # 遍历每个工作表
    for sheet in workbook.sheets():
        try:
            # 尝试将工作表名转为ASCII，如果失败则跳过该工作表
            sheet_name = sheet.name.encode('ascii')
        except UnicodeEncodeError:
            continue
        # 获取第一行的列名（数据名称）
        headers = sheet.row_values(0)[1:]  # 第一行第一列是时间，忽略它
        localTranslater = ReTranslater.copy()
        localTranslater.update({'OP':str(sheet_name)})
        # 遍历每个数据列，构造数据元组
        for col_idx, data_name in enumerate(headers):
            # 获取时间列数据
            time_column = sheet.col_values(0)[1:]  # 排除时间列的标题
            # 获取对应的数据列数据
            data_column = sheet.col_values(col_idx + 1)[1:]  # 数据列，从第二行开始
            
            # 使用zip将时间和数据列组合成二维元组
            data_tuple = tuple(zip(time_column, data_column))
            localTranslater.update({'TT':str(data_name)})
            ampname=parse_string(template,localTranslater)
            # 传递数据元组和依据模板的幅值表名
            data_proceed(data_tuple,ampname)

def parse_string(template,ReTranslater):
    """
    解析模板字符串并替换所有匹配的占位符。

    :param template: 模板字符串，包含类似 %OP%、%NM% 的占位符
    :param ReTranslater: 字典，键为占位符（如 'OP', 'NM'），值为替换内容
    :return: 替换后的字符串
    """
    # 使用正则表达式替换所有 %KEY% 形式的占位符
    def replace_match(match):
        key = match.group(1)  # 提取占位符中的键（如 OP, NM）
        return ReTranslater.get(key, match.group(0))  # 如果键存在则替换，否则保留原占位符
    # 正则表达式匹配所有 %KEY% 形式的占位符
    pattern = re.compile(r'%(\w+)%')
    result = pattern.sub(replace_match, template)
    return result
# 调用函数
if __name__ == '__main__':
    # pre_ampTableChange(u'C:\\Users\\mrvoid\\abaqus_plugins\\STPM_test1035\\管接头工况 下管板.xls',template='%OP%_%TT%')
    pre_ampTableChange(u'C:\\Users\\mrvoid\\abaqus_plugins\\STPM_test1035\\XGB_Data.xls',template='%OP%_%TT%')