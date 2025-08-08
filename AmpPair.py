# -*- coding: utf-8 -*-
"""
Created on Fri Mar  7 14:48:34 2025

@author: mrvoid
！！！注意：需要处理胀接管接头及重启动情况，
# 这是文件的创建日期和作者信息。
# 这是一个重要的注意事项，提示脚本需要处理胀接管接头（Tube Expansion Joint）和重启动（Restart）情况。
# 1、胀接分析步处理（before1-before2）
# 第一点：具体说明了胀接分析步的处理范围， 指的是在某个时间段或特定步骤之间。
# 2、载荷禁用分析步处理
# 第二点：说明需要处理在某些分析步中禁用载荷的情况。
# 3、对应循环条件修改
# 第三点：提示需要修改与循环分析相关的条件。
"""


import re
# 导入re模块，用于正则表达式操作。
from abaqus import *
# 从abaqus模块导入所有内容，包括Abaqus的API对象和函数。
from abaqusConstants import *
# 从abaqusConstants模块导入所有常量，如YES, NO, CANCEL, FROM_FILE等。
from collections import Counter, defaultdict
# 从collections模块导入Counter和defaultdict，用于数据计数和默认字典。

def get_current_model():
    # 定义函数get_current_model，用于获取当前Abaqus会话中活动视口关联的模型。
    """获取当前视口关联的模型"""
    # 函数的文档字符串，简要说明其功能。
    flag=None
    # 初始化flag变量为None，用于存储用户对警告的回复。
    viewport = session.currentViewportName
    # 获取当前会话中活动视口的名称。
    modelname=session.sessionState[viewport]['modelName']
    # 从会话状态中获取当前视口所显示的模型名称。
    if 'Model-0' in modelname:
    # 检查当前模型名称是否包含'Model-0'，这是Abaqus默认模型的名称。
        flag=getWarningReply(
            'WARRNING: Edit Model-0 is not recommanded for user!\n YES-continue; No-copyNew;', (YES,NO,CANCEL))
        # 如果是Model-0，则弹出一个警告框，询问用户是否继续编辑Model-0，或者复制一个新模型。
    if flag==NO:
    # 如果用户选择“No”（不推荐编辑Model-0，选择复制新模型）。
        newname=modelname.replace('Model-0','NewModel')
        # 创建一个新的模型名称，将'Model-0'替换为'NewModel'。
        if newname in mdb.models.keys():
        # 检查新模型名称是否已存在于当前MDB数据库的模型列表中。
            newname=newname+'-Copy'
            # 如果已存在，则在新名称后添加'-Copy'以避免冲突。
        mdb.Model(name=newname, 
                  objectToCopy=mdb.models[modelname])
        # 在MDB数据库中创建一个新模型，通过复制当前Model-0的内容。
        a = mdb.models[newname].rootAssembly
        # 获取新模型的根装配（rootAssembly）。
        session.viewports[viewport].setValues(displayedObject=a)
        # 将当前视口显示的对象设置为新模型的根装配，即切换到新模型。
        return mdb.models[newname]
        # 返回新创建的模型对象。
    elif flag==CANCEL:
    # 如果用户选择“CANCEL”（取消操作）。
        raise Exception('User Cancels when edit {mm}'.format(mm=modelname))
        # 抛出一个异常，表示用户取消了操作。
        return mdb.models[modelname]
        # 理论上这行代码不会执行，因为上面已经抛出异常。
    return mdb.models[modelname]
    # 如果不是Model-0，或者用户选择“YES”继续编辑Model-0，则直接返回当前模型对象。

def modelTypeCheck(m,all_steps):
    # 定义函数modelTypeCheck，用于检查模型的最后一个分析步类型。
    # m: 模型对象。
    # all_steps: 模型中所有分析步的名称列表。
    if all_steps:
    # 检查all_steps列表是否非空。
        modeltype=m.steps[all_steps[-1]].__name__
        # 获取最后一个分析步的类型名称（例如 'HeatTransferStep', 'ViscoStep', 'StaticStep'）。
    else:
    # 如果all_steps列表为空，即模型中没有分析步。
        modeltype=''
        # 将modeltype设置为空字符串。
        raise Exception('no steps for Load/HTC!')
        # 抛出异常，提示没有分析步来定义载荷或热对流。
    return modeltype
    # 返回模型的类型。

def kernelCombine(tabledata,bstep,csteplist,cyctimes,astep,
                  lossDefaultStrategy='Propagated',
                  fieldData=None,
                  preFieldname='ImportedTemperature',
                  importedODBName='.odb'):
    # 定义核心函数kernelCombine，用于组合和管理Abaqus分析步中的载荷、边界条件和预定义场。
    # tabledata: 主要数据表，包含载荷/相互作用的名称和幅值模板。
    # bstep: 初始分析步列表（before steps）。
    # csteplist: 循环分析步列表（cyclic steps）。
    # cyctimes: 循环次数。
    # astep: 结束分析步列表（after steps）。
    # lossDefaultStrategy: 丢失默认策略，可以是'Propagated'（传播）或'Instantaneous'（瞬时）。
    # fieldData: 预定义场数据表，用于导入温度场。
    # preFieldname: 预定义场的名称，默认为'ImportedTemperature'。
    # importedODBName: 导入温度数据来源的ODB文件名，默认为'.odb'。
    if lossDefaultStrategy not in ['Propagated', 'Instantaneous']:
    # 检查lossDefaultStrategy参数是否为有效值。
        raise Exception("lossDefaultStrategy ERROR type!")
        # 如果不是，则抛出类型错误异常。
    m=get_current_model()
    # 获取当前Abaqus模型对象。
    all_steps=[name for name in m.steps.keys() if name != "Initial"]
    # 获取模型中所有非“Initial”分析步的名称列表。
    modeltype=modelTypeCheck(m,all_steps)
    # 检查模型的类型，基于最后一个分析步的类型。
    if modeltype=='HeatTransferStep':
    # 如果模型类型是热传导分析步。
        cyctimes=1
        # 将循环次数强制设置为1，因为热传导通常不进行多循环。
    stepOPlist=bstep+csteplist*cyctimes+astep
    # 构建完整的操作步骤列表：初始步 + 循环步 * 循环次数 + 结束步。
    if len(all_steps) < len(stepOPlist):
    # 检查模型中实际的分析步数量是否少于期望的操作步骤列表长度。
        raise Exception("The number of steps does not match the length of the step action list!")
        # 如果不匹配，则抛出异常。
    elif len(all_steps) > len(stepOPlist):
    # 如果模型中实际的分析步数量多于期望的操作步骤列表长度。
        ns=getInput('The number of steps does not match the length of the step action list!\nPlease specify an offset (ignore the previous steps)',str(len(all_steps) - len(stepOPlist)))
        # 弹出一个输入框，让用户指定一个偏移量，以忽略前面多余的分析步。
        try:
            ns=int(ns)
            # 尝试将用户输入转换为整数。
        except:
            raise Exception("ERROR INPUT")
            # 如果转换失败，则抛出输入错误异常。
        all_steps=all_steps[ns:]
        # 根据用户指定的偏移量，截取all_steps列表，只保留从偏移量开始的分析步。
    if modeltype=='HeatTransferStep':
    # 如果模型类型是热传导分析步。
        heatAmpCombine(zip(all_steps,stepOPlist),tabledata,lossDefaultStrategy)
        # 调用heatAmpCombine函数处理热传导相关的幅值和相互作用。
    elif modeltype=='ViscoStep' or modeltype=='StaticStep':
    # 如果模型类型是粘弹性分析步或静态分析步。
        loadAmpCombine(zip(all_steps,stepOPlist),tabledata,lossDefaultStrategy)
        # 调用loadAmpCombine函数处理载荷相关的幅值。
        if fieldData:
        # 如果提供了预定义场数据。
            stepNDOPlist=rename_duplicates(bstep)+rename_duplicates(csteplist)*cyctimes+rename_duplicates(astep)
            # 为预定义场操作列表生成一个不含重复项的名称列表，用于处理重复步骤名的情况。
            tempPreFieldCombine(zip(all_steps,stepNDOPlist),fieldData,preFieldname,importedODBName)
            # 调用tempPreFieldCombine函数处理温度预定义场。
    else:
    # 如果模型类型不支持。
        raise Exception('Unsupported step type!')
        # 抛出异常，提示不支持的分析步类型。

def loadAmpCombine(stepName_to_stepOP,tabledata,lossDefaultStrategy):
    # 定义函数loadAmpCombine，用于在不同分析步中设置载荷的幅值。
    # stepName_to_stepOP: 一个迭代器，包含 (分析步名称, 操作步骤名称) 对。
    # tabledata: 载荷数据表，包含载荷名称和幅值模板。
    # lossDefaultStrategy: 丢失默认策略。
    m=get_current_model()
    # 获取当前Abaqus模型对象。
    ReTranslater = {}
    # 初始化一个空字典ReTranslater，用于存储模板字符串的替换值。
    for stepname,OP in stepName_to_stepOP:
    # 遍历每个 (分析步名称, 操作步骤名称) 对。
        localTranslater = ReTranslater.copy()
        # 复制全局的ReTranslater字典到localTranslater，以便在当前循环中添加特定值。
        localTranslater.update({'OP':OP})
        # 将当前操作步骤名称OP添加到localTranslater中，键为'OP'。
        for row in tabledata:
        # 遍历tabledata中的每一行，每行代表一个载荷的配置。
            loadname=row[0]
            # 获取载荷名称，位于行的第一个元素。
            localTranslater.update({'NM':loadname})
            # 将当前载荷名称loadname添加到localTranslater中，键为'NM'。
            AmpName=parse_string(row[1],localTranslater)
            # 解析幅值模板字符串（位于行的第二个元素），使用localTranslater进行替换，得到最终的幅值名称。
            if loadname not in m.loads:
            # 检查当前载荷名称是否存在于模型的载荷集合中。
                raise Exception("Load '{0}' does not exist!".format(loadname))
                # 如果载荷不存在，则抛出异常。
            if lossDefaultStrategy == 'Propagated':
            # 如果丢失默认策略是'Propagated'（传播）。
                if AmpName not in m.amplitudes:
                # 如果解析出的幅值名称不存在于模型的幅值集合中。
                    # print('deactivating {}'.format(stepname))
                    # 调试信息：打印正在禁用的分析步。
                    m.loads[loadname].deactivate(stepname)
                    # 在当前分析步中禁用该载荷。
                    # print('reseting {}'.format(stepname))
                    # 调试信息：打印正在重置的分析步。
                    m.loads[loadname].reset(stepname)
                    # 在当前分析步中重置该载荷，使其恢复到前一步的状态。
                else:
                    loads_setValues(m,stepname,loadname,AmpName)
                    # 如果幅值存在，则调用loads_setValues函数设置载荷的幅值。
            elif lossDefaultStrategy == 'Instantaneous':
            # 如果丢失默认策略是'Instantaneous'（瞬时）。
                AmpName = '' if AmpName not in m.amplitudes else AmpName
                # 如果幅值名称不存在，则将其设置为空字符串，否则保持原样。
                loads_setValues(m,stepname,loadname,AmpName)
                # 调用loads_setValues函数设置载荷的幅值。

def tempPreFieldCombine(stepName_to_stepOP,tabledata,preFieldname,importedODBName):
    # 定义函数tempPreFieldCombine，用于处理温度预定义场的导入。
    # stepName_to_stepOP: 一个迭代器，包含 (分析步名称, 操作步骤名称) 对。
    # tabledata: 预定义场数据表，包含操作步骤名称和导入参数。
    # preFieldname: 预定义场的名称。
    # importedODBName: 导入数据的ODB文件名。
    m = get_current_model()
    # 获取当前Abaqus模型对象。
    stepdicts = {row[0]: list(row[1:]) for row in tabledata}
    # 将tabledata转换为字典，键为操作步骤名称，值为导入参数列表。
    has_met_nonone=False
    # 初始化标志has_met_nonone为False，用于跟踪是否遇到过非None的配置。
    stopFlag=None
    # 初始化停止标志stopFlag为None。
    for stepname,OP in stepName_to_stepOP:
    # 遍历每个 (分析步名称, 操作步骤名称) 对。
        try:
            configs=stepdicts[OP]
            # 尝试从stepdicts中获取当前操作步骤的配置。
        except KeyError: #当OP不在用户传入数据中
            # 如果操作步骤名称OP不在stepdicts中（即用户传入数据中没有该OP）。
            configs=[None]*4 #视为全None
            # 则将配置视为全None。
        if has_met_nonone:
        # 如果之前已经遇到过非None的配置。
            stopFlag=predefinedFields_setValues(m,stepname,configs,importedODBName,preFieldname)
            # 调用predefinedFields_setValues函数设置预定义场，并获取停止标志。
        else:
        # 如果之前没有遇到过非None的配置。
            if configs!=[None]*4:
            # 如果当前配置不是全None。
                has_met_nonone=True
                # 将has_met_nonone标志设置为True。
                stopFlag=predefinedFields_setValues(m,stepname,configs,importedODBName,preFieldname)
                # 调用predefinedFields_setValues函数设置预定义场，并获取停止标志。
        if stopFlag=='Stop':
        # 如果停止标志为'Stop'。
            break
            # 终止循环。

def heatAmpCombine(stepName_to_stepOP,tabledata,lossDefaultStrategy):
    # 定义函数heatAmpCombine，用于在不同分析步中设置热对流（Film）和辐射（Sink）相互作用的幅值。
    # stepName_to_stepOP: 一个迭代器，包含 (分析步名称, 操作步骤名称) 对。
    # tabledata: 热对流数据表，包含相互作用名称和幅值模板。
    # lossDefaultStrategy: 丢失默认策略。
    m=get_current_model()
    # 获取当前Abaqus模型对象。
    ReTranslater = {}
    # 初始化一个空字典ReTranslater，用于存储模板字符串的替换值。
    for stepname,OP in stepName_to_stepOP:
    # 遍历每个 (分析步名称, 操作步骤名称) 对。
        localTranslater = ReTranslater.copy()
        # 复制全局的ReTranslater字典到localTranslater。
        localTranslater.update({'OP':OP})
        # 将当前操作步骤名称OP添加到localTranslater中。
        for row in tabledata:
        # 遍历tabledata中的每一行，每行代表一个相互作用的配置。
            intername=row[0]
            # 获取相互作用名称，位于行的第一个元素。
            localTranslater.update({'NM':intername})
            # 将当前相互作用名称intername添加到localTranslater中。
            FilmAmpName=parse_string(row[1],localTranslater)
            # 解析Film幅值模板字符串，得到Film幅值名称。
            SinkAmpName=parse_string(row[2],localTranslater)
            # 解析Sink幅值模板字符串，得到Sink幅值名称。
            if lossDefaultStrategy == 'Propagated':
            # 如果丢失默认策略是'Propagated'。
                if FilmAmpName not in m.amplitudes or SinkAmpName not in m.amplitudes:
                # 如果Film幅值或Sink幅值中有一个不存在于模型的幅值集合中。
                    # print('{},{} not in amp, stepname:{}'.format(FilmAmpName,SinkAmpName,stepname))
                    # 调试信息：打印不存在的幅值和分析步。
                    try:
                        m.interactions[intername].deactivate(stepname)
                        # 尝试在当前分析步中禁用该相互作用。
                        m.interactions[intername].reset(stepname)
                        # 尝试在当前分析步中重置该相互作用。
                    except:
                        pass
                        # 如果禁用或重置失败（例如相互作用在该步不活跃），则忽略错误。
                else:
                    interact_setValues(m, stepname, intername, FilmAmpName, SinkAmpName)
                    # 如果两个幅值都存在，则调用interact_setValues函数设置相互作用的幅值。
            elif lossDefaultStrategy == 'Instantaneous':
            # 如果丢失默认策略是'Instantaneous'。
                FilmAmpName = '' if FilmAmpName not in m.amplitudes else FilmAmpName
                # 如果Film幅值不存在，则将其设置为空字符串。
                SinkAmpName = '' if SinkAmpName not in m.amplitudes else SinkAmpName
                # 如果Sink幅值不存在，则将其设置为空字符串。
                interact_setValues(m, stepname, intername, FilmAmpName, SinkAmpName)
                # 调用interact_setValues函数设置相互作用的幅值。

def interact_setValues(m,stepname,intername,FilmAmpName,SinkAmpName):
    # 定义函数interact_setValues，用于在指定分析步中设置相互作用的Film和Sink幅值。
    # m: 模型对象。
    # stepname: 分析步名称。
    # intername: 相互作用名称。
    # FilmAmpName: Film幅值名称。
    # SinkAmpName: Sink幅值名称。
    try:
        m.interactions[intername].setValuesInStep(
            stepName=stepname,
            filmCoeffAmplitude=FilmAmpName, 
            sinkAmplitude=SinkAmpName)
        # 尝试在指定分析步中设置相互作用的Film系数幅值和Sink幅值。
    except Exception as e:
    # 捕获 发生的异常。
        if str(e)=='Interaction is not active in this step.':
        # 如果异常信息是“相互作用在该步不活跃”。
            print('{e} : {i} {s}'.format(e=str(e),i=intername,s=stepname))
            # 打印警告信息，说明相互作用在该步不活跃。

def loads_setValues(m,stepname,loadname,AmpName):
    # 定义函数loads_setValues，用于在指定分析步中设置载荷的幅值。
    # m: 模型对象。
    # stepname: 分析步名称。
    # loadname: 载荷名称。
    # AmpName: 幅值名称。
    try:
        m.loads[loadname].setValuesInStep(
            stepName=stepname, amplitude=AmpName)
        # 尝试在指定分析步中设置载荷的幅值。
    except Exception as e:
    # 捕获 发生的异常。
        if str(e)=='The load does not exist in the specifed step or is suppressed or inactive.':
        # 如果异常信息是“载荷在指定步中不存在或被抑制或不活跃”。
            print('{e} : {l} {s}'.format(e=str(e),l=loadname,s=stepname))
            # 打印警告信息，说明载荷在该步中存在问题。

def process_element(element):
    # 定义函数process_element，用于处理预定义场导入参数的单个元素。
    # element: 输入的元素， 是字符串或数字。
    if element == 'FREED':# or element == '' 
    # 如果元素是字符串'FREED'。
        return FREED
        # 返回Abaqus常量FREED。
    elif element in ['Propagated','Initial']:
    # 如果元素是字符串'Propagated'或'Initial'。
        return element
        # 直接返回该字符串。
    elif element=='-1':
    # 如果元素是字符串'-1'。
        element=int(1E6)
        # 将其转换为一个非常大的整数（100万）， 代表“直到结束”。
        return element
        # 返回转换后的整数。
    try:
        return int(element)
        # 尝试将元素转换为整数并返回。
    except (ValueError, TypeError):
    # 如果转换失败（例如元素不是有效的数字字符串）。
        return None
        # 返回None。

def predefinedFields_setValues(m,stepname,numbers,fileName,preFieldname):
    # 定义函数predefinedFields_setValues，用于设置预定义场（如温度场）。
    # m: 模型对象。
    # stepname: 分析步名称。
    # numbers: 包含导入参数的列表（beginStep, beginIncrement, endStep, endIncrement）。
    # fileName: 导入数据的ODB文件名。
    # preFieldname: 预定义场的名称。
    numbers=[process_element(element) for element in numbers]
    # 对numbers列表中的每个元素进行处理，转换为Abaqus可识别的格式。
    if preFieldname not in m.predefinedFields:
    # 如果预定义场名称不在模型的预定义场集合中。
        m.Temperature(name=preFieldname, 
            createStepName=stepname, distributionType=FROM_FILE, fileName=fileName, 
            beginStep=numbers[0], beginIncrement=numbers[1], endStep=numbers[2], endIncrement=numbers[3], 
            interpolate=OFF, absoluteExteriorTolerance=0.0, exteriorTolerance=0.05)
        # 创建一个新的温度预定义场，指定其名称、创建步、分布类型（从文件）、文件名以及导入的步/增量信息。
        # interpolate=OFF表示不进行插值，absoluteExteriorTolerance和exteriorTolerance是外部容差设置。
        for opf in m.predefinedFields.keys():
        # 遍历模型中所有预定义场的名称。
            if m.predefinedFields[opf].__name__=='Temperature':
            # 如果当前预定义场是温度类型。
                try:
                    m.predefinedFields[opf].resetToInitial(
                        stepName=stepname)
                    # 尝试将该温度预定义场重置到初始状态（ 用于清除之前设置的温度场）。
                except:
                    pass
                    # 如果重置失败，则忽略错误。
    else:
    # 如果预定义场名称已经存在于模型的预定义场集合中。
        m.predefinedFields[preFieldname].setValues(fileName=fileName)
        # 更新现有预定义场的源文件名。
    if numbers[0]=='Propagated':
    # 如果导入参数的第一个元素是'Propagated'。
        m.predefinedFields[preFieldname].resetToPropagated(
            stepName=stepname)
        # 将预定义场重置为从前一步传播而来。
    elif numbers[0]=='Initial':
    # 如果导入参数的第一个元素是'Initial'。
        m.predefinedFields[preFieldname].resetToInitial(
            stepName=stepname)
        # 将预定义场重置为初始状态。
        return 'Stop'
        # 返回'Stop'标志，表示可以停止后续处理。
    else:
    # 如果导入参数是具体的步/增量信息。
        m.predefinedFields[preFieldname].setValuesInStep(
            stepName=stepname,
            beginStep=numbers[0],beginIncrement=numbers[1],
            endStep=numbers[2], endIncrement=numbers[3])
        # 在指定分析步中设置预定义场的导入范围（起始步、起始增量、结束步、结束增量）。


def parse_string(template,ReTranslater):
    # 定义函数parse_string，用于解析模板字符串并替换占位符。
    """
    解析模板字符串并替换所有匹配的占位符。

    :param template: 模板字符串，包含类似 %OP%、%NM% 的占位符
    :param ReTranslater: 字典，键为占位符（如 'OP', 'NM'），值为替换内容
    :return: 替换后的字符串
    """
    # 函数的文档字符串，详细说明其功能、参数和返回值。
    # 使用正则表达式替换所有 %KEY% 形式的占位符
    # 这是一个内部函数，用于处理正则表达式匹配到的内容。
    def replace_match(match):
        key = match.group(1)  # 提取占位符中的键（如 OP, NM）
        # 从匹配对象中提取括号内的内容，即占位符的键。
        return ReTranslater.get(key, match.group(0))  # 如果键存在则替换，否则保留原占位符
        # 从ReTranslater字典中获取键对应的值，如果键不存在，则返回原始匹配的字符串（即不替换）。
    # 正则表达式匹配所有 %KEY% 形式的占位符
    pattern = re.compile(r'%(\w+)%')
    # 编译正则表达式，匹配以'%'开头，后跟一个或多个字母数字字符（\w+），再以'%'结尾的模式。
    # 括号()创建了一个捕获组，用于提取键。
    result = pattern.sub(replace_match, template)
    # 使用re.sub方法进行替换，将模板字符串中所有匹配到的模式替换为replace_match函数返回的值。
    return result
    # 返回替换后的字符串。

def rename_duplicates(lst):
    # 定义函数rename_duplicates，用于处理列表中重复的元素，为其添加后缀以使其唯一。
    # lst: 输入列表。
    # 统计每个元素的总出现次数
    freq = Counter(lst)
    # 使用collections.Counter统计列表中每个元素的出现频率。
    # 用于记录每个重复元素目前出现的次数
    occurrence = defaultdict(int)
    # 使用collections.defaultdict创建一个字典，用于记录每个元素当前出现的次数，默认值为0。
    result = []
    # 初始化一个空列表，用于存储处理后的结果。
    for item in lst:
    # 遍历输入列表中的每个元素。
        # 如果元素出现多次，则添加后缀
        if freq[item] > 1:
        # 如果当前元素的总出现次数大于1（即是重复元素）。
            occurrence[item] += 1
            # 增加该元素当前出现的次数计数。
            result.append("{}-{}".format(item, occurrence[item]))
            # 将元素名称和其当前出现次数作为后缀拼接，并添加到结果列表中。
        else:
            result.append(item)
            # 如果元素不重复，则直接添加到结果列表中。
    return result
    # 返回处理后的列表。

if __name__=='__main__':
    # 这是一个Python程序的入口点，当脚本直接运行时，此块中的代码将被执行。
    ###管接头数据
    # 这是一个注释，指示以下数据是针对“管接头”的配置。
    # tabledata1=(
    #     # ('HTC1','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
    #     ('HTC2','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
    #     ('Ra','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
    #     ('TPoutHTC1','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
    #     ('TPoutHTCSo_Origin','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),)
    # 这是一个被注释掉的示例数据，用于热对流（HTC）相关的配置。
    # 每行包含：相互作用名称，Film幅值模板，Sink幅值模板。
    # tabledata2=(
    #     ('Balance','%OP%_%NM%'),
    #     ('ForcePipeAx','%OP%_%NM%'),
    #     ('ForceShear','%OP%_%NM%'),
    #     ('NaPressure','%OP%_%NM%'),
    #     ('WaterPressure','%OP%_%NM%'),
    #     )
    # 这是一个被注释掉的示例数据，用于载荷相关的配置。
    # 每行包含：载荷名称，幅值模板。
    ###管板数据
    # 这是一个注释，指示以下数据是针对“管板”的配置。
    tabledata1=(
        ('JG','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
        # JG相互作用的热对流和温度幅值模板。
        ('HTC2','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
        # HTC2相互作用的热对流和温度幅值模板。
        ('Ra','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
        # Ra相互作用的热对流和温度幅值模板。
        ('TPoutHTC1','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),
        # TPoutHTC1相互作用的热对流和温度幅值模板。
        ('TPoutHTCSo_Origin','%OP%_%NM%_HTC','%OP%_%NM%_TEMP'),)
        # TPoutHTCSo_Origin相互作用的热对流和温度幅值模板。
    tabledata2=(
        ('BalanceGB','%OP%_%NM%'),
        # BalanceGB载荷的幅值模板。
        ('BalanceJG','%OP%_%NM%'),
        # BalanceJG载荷的幅值模板。
        ('ForceJGAx','%OP%_%NM%'),
        # ForceJGAx载荷的幅值模板。
        ('ForceJGBD1','%OP%_%NM%'),
        # ForceJGBD1载荷的幅值模板。
        ('ForceJGBD2','%OP%_%NM%'),
        # ForceJGBD2载荷的幅值模板。
        ('ForceJGTOR','%OP%_%NM%'),
        # ForceJGTOR载荷的幅值模板。
        ('ForceJGSH1','%OP%_%NM%'),
        # ForceJGSH1载荷的幅值模板。
        ('ForceJGSH2','%OP%_%NM%'),
        # ForceJGSH2载荷的幅值模板。
        ('ForceShear','%OP%_%NM%'),
        # ForceShear载荷的幅值模板。
        ('NaPressure','%OP%_%NM%'),
        # NaPressure载荷的幅值模板。
        ('WaterPressure','%OP%_%NM%'),
        # WaterPressure载荷的幅值模板。
        )
    tabledata3=(
        ('G5','1','0','1','-1'),
        # G5步骤的温度导入配置：从ODB的第1步第0增量到第1步的最后一个增量。
        ('Steady-1','2','0','2','-1'),
        # Steady-1步骤的温度导入配置：从ODB的第2步第0增量到第2步的最后一个增量。
        ('G6','3','0','3','-1'),
        # G6步骤的温度导入配置：从ODB的第3步第0增量到第3步的最后一个增量。
        ('Steady-2','4','0','4','-1'),
        # Steady-2步骤的温度导入配置：从ODB的第4步第0增量到第4步的最后一个增量。
        ('G13','5','0','5','-1'),
        # G13步骤的温度导入配置：从ODB的第5步第0增量到第5步的最后一个增量。
        ('Steady-3','6','0','6','-1'),
        # Steady-3步骤的温度导入配置：从ODB的第6步第0增量到第6步的最后一个增量。
        ('G9','7','0','7','-1'),
        # G9步骤的温度导入配置：从ODB的第7步第0增量到第7步的最后一个增量。
        ('HOLDING','Propagated','','',''),
        # HOLDING步骤的温度导入配置：温度场从前一步传播。
        # ('HOLDING','9','0','9','-1'),
        # 这是一个被注释掉的HOLDING步骤的另一种温度导入配置。
        )
    tabledata=tabledata2
    # 将tabledata设置为tabledata2，表示当前运行将处理载荷相关的配置。
    bstep = []
    # 定义初始分析步列表bstep，当前为空。
    csteplist = ['G5','Steady','G6','Steady','G13','Steady','G9']
    # 定义循环分析步列表csteplist，包含一系列的G步和Steady步。
    astep = ['G5','HOLDING']
    # 定义结束分析步列表astep。
    cyctimes = 5
    # 定义循环次数为5。
    kernelCombine(tabledata,bstep,csteplist,cyctimes,astep,
                  lossDefaultStrategy='Propagated',
                  fieldData=tabledata3,
                  preFieldname='ImportedTemperature',
                  importedODBName='GB-HeatTransfer.odb')
    # 调用核心函数kernelCombine执行操作。
    # 参数包括：当前选择的载荷数据表tabledata，初始步bstep，循环步csteplist，循环次数cyctimes，结束步astep。
    # lossDefaultStrategy设置为'Propagated'，表示默认情况下载荷/相互作用会从前一步传播。
    # fieldData设置为tabledata3，用于导入温度预定义场。
    # preFieldname设置为'ImportedTemperature'，指定预定义场的名称。
    # importedODBName设置为'GB-HeatTransfer.odb'，指定温度数据来源的ODB文件。