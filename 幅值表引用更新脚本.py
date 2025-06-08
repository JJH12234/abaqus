# -*- coding: utf-8 -*-
"""
Created on Fri Mar  7 14:48:34 2025

@author: mrvoid
！！！注意：需要处理胀接管接头及重启动情况，
1、胀接分析步处理（before1-before2）
2、载荷禁用分析步处理
3、对应循环条件修改
……
"""


import re
from abaqus import *
from abaqusConstants import *
from collections import Counter, defaultdict
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
def modelTypeCheck(m,all_steps):
    if all_steps:
        modeltype=m.steps[all_steps[-1]].__name__
    else:
        modeltype=''
        raise Exception('no steps for Load/HTC!')
    return modeltype

def kernelCombine(tabledata,bstep,csteplist,cyctimes,astep,
                  lossDefaultStrategy='Propagated',
                  fieldData=None,
                  preFieldname='ImportedTemperature',
                  importedODBName='.odb'):
    if lossDefaultStrategy not in ['Propagated', 'Instantaneous']:
        raise Exception("lossDefaultStrategy ERROR type!")
    m=get_current_model()
    all_steps=[name for name in m.steps.keys() if name != "Initial"]
    modeltype=modelTypeCheck(m,all_steps)
    if modeltype=='HeatTransferStep':
        cyctimes=1
    stepOPlist=bstep+csteplist*cyctimes+astep
    if len(all_steps) < len(stepOPlist):
        raise Exception("The number of steps does not match the length of the step action list!")
    elif len(all_steps) > len(stepOPlist):
        ns=getInput('The number of steps does not match the length of the step action list!\nPlease specify an offset (ignore the previous steps)',str(len(all_steps) - len(stepOPlist)))
        try:
            ns=int(ns)
        except:
            raise Exception("ERROR INPUT")
        all_steps=all_steps[ns:]
    if modeltype=='HeatTransferStep':
        heatAmpCombine(zip(all_steps,stepOPlist),tabledata,lossDefaultStrategy)
    elif modeltype=='ViscoStep' or modeltype=='StaticStep':
        loadAmpCombine(zip(all_steps,stepOPlist),tabledata,lossDefaultStrategy)
        if fieldData:
            stepNDOPlist=rename_duplicates(bstep)+rename_duplicates(csteplist)*cyctimes+rename_duplicates(astep)
            tempPreFieldCombine(zip(all_steps,stepNDOPlist),fieldData,preFieldname,importedODBName)
    else:
        raise Exception('Unsupported step type!')

def loadAmpCombine(stepName_to_stepOP,tabledata,lossDefaultStrategy):
    m=get_current_model()
    ReTranslater = {}
    for stepname,OP in stepName_to_stepOP:
        localTranslater = ReTranslater.copy()
        localTranslater.update({'OP':OP})
        for row in tabledata:
            loadname=row[0]
            localTranslater.update({'NM':loadname})
            AmpName=parse_string(row[1],localTranslater)
            if loadname not in m.loads:
                raise Exception("Load '{0}' does not exist!".format(loadname))
            if lossDefaultStrategy == 'Propagated':
                if AmpName not in m.amplitudes:
                    # print('deactivating {}'.format(stepname))
                    m.loads[loadname].deactivate(stepname)
                    # print('reseting {}'.format(stepname))
                    m.loads[loadname].reset(stepname)
                else:
                    loads_setValues(m,stepname,loadname,AmpName)
            elif lossDefaultStrategy == 'Instantaneous':
                AmpName = '' if AmpName not in m.amplitudes else AmpName
                loads_setValues(m,stepname,loadname,AmpName)

def tempPreFieldCombine(stepName_to_stepOP,tabledata,preFieldname,importedODBName):
    m = get_current_model()
    stepdicts = {row[0]: list(row[1:]) for row in tabledata}
    has_met_nonone=False
    stopFlag=None
    for stepname,OP in stepName_to_stepOP:
        try:
            configs=stepdicts[OP]
        except KeyError: #当OP不在用户传入数据中
            configs=[None]*4 #视为全None
        if has_met_nonone:
            stopFlag=predefinedFields_setValues(m,stepname,configs,importedODBName,preFieldname)
        else:
            if configs!=[None]*4:
                has_met_nonone=True
                stopFlag=predefinedFields_setValues(m,stepname,configs,importedODBName,preFieldname)
        if stopFlag=='Stop':
            break
def heatAmpCombine(stepName_to_stepOP,tabledata,lossDefaultStrategy):
    m=get_current_model()
    ReTranslater = {} 
    for stepname,OP in stepName_to_stepOP:
        localTranslater = ReTranslater.copy()
        localTranslater.update({'OP':OP})
        for row in tabledata:
            intername=row[0]
            localTranslater.update({'NM':intername})
            FilmAmpName=parse_string(row[1],localTranslater)
            SinkAmpName=parse_string(row[2],localTranslater)
            if lossDefaultStrategy == 'Propagated':
                if FilmAmpName not in m.amplitudes or SinkAmpName not in m.amplitudes:
                    # print('{},{} not in amp, stepname:{}'.format(FilmAmpName,SinkAmpName,stepname))
                    try:
                        m.interactions[intername].deactivate(stepname)
                        m.interactions[intername].reset(stepname)
                    except:
                        pass
                else:
                    interact_setValues(m, stepname, intername, FilmAmpName, SinkAmpName)
            elif lossDefaultStrategy == 'Instantaneous':
                FilmAmpName = '' if FilmAmpName not in m.amplitudes else FilmAmpName
                SinkAmpName = '' if SinkAmpName not in m.amplitudes else SinkAmpName
                interact_setValues(m, stepname, intername, FilmAmpName, SinkAmpName)

def interact_setValues(m,stepname,intername,FilmAmpName,SinkAmpName):
    try:
        m.interactions[intername].setValuesInStep(
            stepName=stepname,
            filmCoeffAmplitude=FilmAmpName, 
            sinkAmplitude=SinkAmpName)
    except Exception as e:
        if str(e)=='Interaction is not active in this step.':
            print('{e} : {i} {s}'.format(e=str(e),i=intername,s=stepname))

def loads_setValues(m,stepname,loadname,AmpName):
    try:
        m.loads[loadname].setValuesInStep(
            stepName=stepname, amplitude=AmpName)
    except Exception as e:
        if str(e)=='The load does not exist in the specifed step or is suppressed or inactive.':
            print('{e} : {l} {s}'.format(e=str(e),l=loadname,s=stepname))
def process_element(element):
    if element == 'FREED':# or element == '' 
        return FREED
    elif element in ['Propagated','Initial']:
        return element
    elif element=='-1':
        element=int(1E6)
        return element
    try:
        return int(element)
    except (ValueError, TypeError):
        return None

def predefinedFields_setValues(m,stepname,numbers,fileName,preFieldname):
    numbers=[process_element(element) for element in numbers]
    if preFieldname not in m.predefinedFields:
        m.Temperature(name=preFieldname, 
            createStepName=stepname, distributionType=FROM_FILE, fileName=fileName, 
            beginStep=numbers[0], beginIncrement=numbers[1], endStep=numbers[2], endIncrement=numbers[3], 
            interpolate=OFF, absoluteExteriorTolerance=0.0, exteriorTolerance=0.05)
        for opf in m.predefinedFields.keys():
            if m.predefinedFields[opf].__name__=='Temperature':
                try:
                    m.predefinedFields[opf].resetToInitial(
                        stepName=stepname)
                except:
                    pass
    else:
        m.predefinedFields[preFieldname].setValues(fileName=fileName)
    if numbers[0]=='Propagated':
        m.predefinedFields[preFieldname].resetToPropagated(
            stepName=stepname)
    elif numbers[0]=='Initial':
        m.predefinedFields[preFieldname].resetToInitial(
            stepName=stepname)
        return 'Stop'
    else:
        m.predefinedFields[preFieldname].setValuesInStep(
            stepName=stepname,
            beginStep=numbers[0],beginIncrement=numbers[1],
            endStep=numbers[2], endIncrement=numbers[3])


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

def rename_duplicates(lst):
    # 统计每个元素的总出现次数
    freq = Counter(lst)
    # 用于记录每个重复元素目前出现的次数
    occurrence = defaultdict(int)
    result = []
    for item in lst:
        # 如果元素出现多次，则添加后缀
        if freq[item] > 1:
            occurrence[item] += 1
            result.append("{}-{}".format(item, occurrence[item]))
        else:
            result.append(item)
    return result

if __name__=='__main__':
    ###管接头数据
    # tabledata1=(
    #     # ('HTC1','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
    #     ('HTC2','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
    #     ('Ra','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
    #     ('TPoutHTC1','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
    #     ('TPoutHTCSo_Origin','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),)
    # tabledata2=(
    #     ('Balance','%OP%_%NM%'),
    #     ('ForcePipeAx','%OP%_%NM%'),
    #     ('ForceShear','%OP%_%NM%'),
    #     ('NaPressure','%OP%_%NM%'),
    #     ('WaterPressure','%OP%_%NM%'),
    #     )
    ###管板数据
    tabledata1=(
        ('JG','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
        ('HTC2','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
        ('Ra','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
        ('TPoutHTC1','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
        ('TPoutHTCSo_Origin','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),)
    tabledata2=(
        ('BalanceGB','%OP%_%NM%'),
        ('BalanceJG','%OP%_%NM%'),
        ('ForceJGAx','%OP%_%NM%'),
        ('ForceJGBD1','%OP%_%NM%'),
        ('ForceJGBD2','%OP%_%NM%'),
        ('ForceJGTOR','%OP%_%NM%'),
        ('ForceJGSH1','%OP%_%NM%'),
        ('ForceJGSH2','%OP%_%NM%'),
        ('ForceShear','%OP%_%NM%'),
        ('NaPressure','%OP%_%NM%'),
        ('WaterPressure','%OP%_%NM%'),
        )
    tabledata3=(
        ('G5','1','0','1','-1'),
        ('Steady-1','2','0','2','-1'),
        ('G6','3','0','3','-1'),
        ('Steady-2','4','0','4','-1'),
        ('G13','5','0','5','-1'),
        ('Steady-3','6','0','6','-1'),
        ('G9','7','0','7','-1'),
        ('HOLDING','Propagated','','',''),
        # ('HOLDING','9','0','9','-1'),
        )
    tabledata=tabledata2
    bstep = []
    csteplist = ['G5','Steady','G6','Steady','G13','Steady','G9']
    astep = ['G5','HOLDING']
    cyctimes = 5
    kernelCombine(tabledata,bstep,csteplist,cyctimes,astep,
                  lossDefaultStrategy='Propagated',
                  fieldData=tabledata3,
                  preFieldname='ImportedTemperature',
                  importedODBName='GB-HeatTransfer.odb')