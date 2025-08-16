# -*- coding: utf-8 -*-
# 这是一个Python脚本的编码声明，指定使用UTF-8编码。
"""
Created on Fri Mar  7 02:00:24 2025

@author: mrvoid
"""
# 导入操作系统模块，用于文件和目录操作。
import os
# 导入glob模块，用于查找符合特定模式的文件路径名。
import glob
# 导入re模块，用于正则表达式操作。
import re
# 从abaqus模块导入所有内容，这是Abaqus脚本编程的核心。
from abaqus import *
# 从abaqusConstants模块导入所有常量，如ON, OFF等。
from abaqusConstants import *
# 从collections模块导入Counter和defaultdict，用于数据计数和默认字典。
from collections import Counter, defaultdict

# 定义一个辅助函数，用于重命名列表中的重复项，使其唯一。
# 例如：['a', 'b', 'a'] -> ['a', 'b', 'a-1']
def rename_duplicates(items):
    # 使用Counter来统计每个元素的出现次数。
    counts = Counter()
    # 存储处理后的唯一名称列表。
    result = []
    # 遍历原始列表中的每个项。
    for item in items:
        # 增加当前项的计数。
        counts[item] += 1
        # 如果当前项的计数大于1，说明它是重复项。
        if counts[item] > 1:
            # 在项后面添加一个序号，使其唯一。
            result.append(f"{item}-{counts[item]-1}")
        else:
            # 如果是第一次出现，直接添加。
            result.append(item)
    # 返回处理后的列表。
    return result

# 定义一个辅助函数，用于根据模型类型和弹性标志确定场输出变量。
def fieldOutputRequestsMap(m,modeltype):
    # 初始化一个空列表，用于存储要输出的变量。
    variables=[]
    # 遍历模型中所有材料的名称。
    for mname in m.materials.keys():
        # 获取当前材料对象。
        mm=m.materials[mname]
        # 检查材料是否定义了用户输出变量（UVARM）。
        if getattr(mm,'userOutputVariables',None):
            # 如果有，则添加'UVARM'到变量列表。
            variables.append('UVARM')
            # 找到后即可跳出循环，因为只需要添加一次。
            break
    # 再次遍历模型中所有材料的名称。
    for mname in m.materials.keys():
        # 获取当前材料对象。
        mm=m.materials[mname]
        # 检查材料是否定义了用户自定义场（STATUS）。
        if getattr(mm,'userDefinedField',None):
            # 如果有，则添加'STATUS'到变量列表。
            variables.append('STATUS')
            # 找到后即可跳出循环。
            break
    # 根据模型类型添加特定的场输出变量。
    # 如果模型类型是粘弹性步（ViscoStep）。
    if modeltype=='ViscoStep':
        # 添加一系列与粘弹性分析相关的标准输出变量。
        variables.extend([
            'S', 'TRIAX', 'E', 'PE', 'PEEQ', 'PEMAG', 'IE', 'CE', 'CEEQ', 'CEMAG',
            'THE', 'LE', 'U', 'RF', 'CF', 'CSTRESS', 'CDISP', 'NT', 'TEMP', 'COORD', ]
            )
        # 将变量列表转换为元组并返回。
        return tuple(variables)
    # 如果模型类型是静态步（StaticStep）。
    elif modeltype=='StaticStep':
        # 添加一系列与静态分析相关的标准输出变量。
        variables.extend([
            'S', 'E', 'PE', 'PEEQ', 'PEMAG', 'U', 'RF', 'CF', 'COORD', ]
            )
        # 将变量列表转换为元组并返回。
        return tuple(variables)
    # 如果模型类型是传热步（HeatTransferStep）。
    elif modeltype=='HeatTransferStep':
        # 添加一系列与传热分析相关的标准输出变量。
        variables.extend([
            'NT', 'TEMP', 'HFL', 'HFLM', 'HTL', 'HTLM', 'TEMP', 'COORD', ]
            )
        # 将变量列表转换为元组并返回。
        return tuple(variables)
    # 如果是其他模型类型，则返回当前已收集的变量。
    return tuple(variables)

# 定义一个辅助函数，用于根据模型类型返回创建分析步的方法和默认参数。
def stepBuildMap(modeltype):
    # 初始化一个空字典，用于存储分析步的参数。
    paras={}
    # 根据传入的模型类型选择对应的Abaqus分析步创建方法。
    if modeltype=='StaticStep':
        # 静态通用分析步。
        method='StaticStep'
        # 设置默认参数，如时间比例因子。
        paras={'timePeriod':1.0, 'timeIncrementationMethod':AUTOMATIC, 'maxNumInc':1000000}
    elif modeltype=='ViscoStep':
        # 粘弹性分析步。
        method='ViscoStep'
        # 设置默认参数。
        paras={'timePeriod':1.0, 'timeIncrementationMethod':AUTOMATIC, 'maxNumInc':1000000}
    elif modeltype=='HeatTransferStep':
        # 传热分析步。
        method='HeatTransferStep'
        # 设置默认参数。
        paras={'timePeriod':1.0, 'timeIncrementationMethod':AUTOMATIC, 'maxNumInc':1000000}
    elif modeltype=='CoupledThermalElectricStep':
        # 热电耦合分析步。
        method='CoupledThermalElectricStep'
        # 设置默认参数。
        paras={'timePeriod':1.0, 'timeIncrementationMethod':AUTOMATIC, 'maxNumInc':1000000}
    else:
        # 如果模型类型未知，则抛出异常。
        raise Exception('Unknown modeltype: {0}'.format(modeltype))
    # 返回方法名和参数字典。
    return method, paras

# 定义一个辅助函数，用于修改现有分析步的属性。
def stepModifyMap(stepname,edittype,value):
    # 获取当前模型。
    m = get_current_model()
    # 获取指定名称的分析步对象。
    step=m.steps[stepname]
    # 根据修改类型（edittype）来更新分析步的属性。
    if edittype=='timePeriod':
        # 修改时间周期。
        step.setValues(timePeriod=value)
    elif edittype=='initialInc':
        # 修改初始增量步。
        step.setValues(initialInc=value)
    elif edittype=='maxInc':
        # 修改最大增量步。
        step.setValues(maxInc=value)
    elif edittype=='nlgeom':
        # 修改几何非线性设置。
        step.setValues(nlgeom=value)
    else:
        # 如果修改类型未知，则抛出异常。
        raise Exception('Unknown edittype: {0}'.format(edittype))

# 定义获取当前视口关联模型的函数。
def get_current_model():
    # 初始化一个标志变量。
    flag=None
    # 获取当前视口的名称。
    viewport = session.currentViewportName
    # 从会话状态中获取当前视口关联的模型名称。
    modelname=session.sessionState[viewport]['modelName']
    # 检查模型名称是否包含'Model-0'，这是Abaqus的默认模型名称。
    if 'Model-0' in modelname:
        # 如果是Model-0，打印警告信息，提示用户不建议直接编辑。
        # 使用.encode('GB18030')是为了在某些Abaqus版本中正确显示中文。
        print(u'请在弹出窗口执行操作（如未见弹窗可能被遮挡）'.encode('GB18030'))
        # 弹出警告对话框，询问用户是否继续、复制或取消。
        flag=getWarningReply(
            u'警告：不建议用户自行编辑Model-0 \n YES以继续（可能意外导致崩溃）; No以复制;'.encode('GB18030'), (YES,NO,CANCEL))
    # 如果用户选择“否”（NO），表示复制模型。
    if flag==NO:
        # 创建一个新的模型名称，将'Model-0'替换为'NewModel'。
        newname=modelname.replace('Model-0','NewModel')
        # 检查新名称是否已存在于模型数据库中。
        if newname in mdb.models.keys():
            # 如果已存在，则在新名称后添加'-Copy'以避免冲突。
            newname=newname+'-Copy'
        # 创建一个新模型，通过复制现有模型来实现。
        mdb.Model(name=newname,
                  objectToCopy=mdb.models[modelname])
        # 获取新模型的根装配体。
        a = mdb.models[newname].rootAssembly
        # 将当前视口显示的对象设置为新模型的装配体。
        session.viewports[viewport].setValues(displayedObject=a)
        # 返回新创建的模型对象。
        return mdb.models[newname]
    # 如果用户选择“取消”（CANCEL）。
    elif flag==CANCEL:
        # 抛出一个异常，表示用户取消了操作。
        raise Exception('User Cancels when edit {mm}'.format(mm=modelname))
        # return mdb.models[modelname] # 这行代码被注释掉了，不会执行。
    # 如果用户选择“是”（YES）或没有弹出警告（即不是Model-0）。
    else:
        # 继续执行，不做特殊处理。
        pass
    # 返回原始模型对象（如果不是Model-0，或者用户选择继续编辑Model-0）。
    return mdb.models[modelname]

# 定义一个函数，用于生成完整的分析步列表和对应的时间周期。
def fulllist(bstep, csteplist, steptimepair, astep, cyctimes):
    # 构建所有分析步的原始名称列表：前置步 + 循环步 * 循环次数 + 后置步。
    all_steps=bstep+csteplist*cyctimes+astep
    # 根据steptimepair字典为每个分析步确定时间周期。
    # 如果分析步名称在字典中，则使用其对应的时间；否则使用通配符'*'的时间。
    timelist = [steptimepair[x] if x in steptimepair else steptimepair['*'] for x in all_steps]
    # 构建最终的分析步名称列表，处理重复名称并添加前缀/后缀。
    steplist = (
        # 为前置步添加'before-'前缀，并处理重复名称。
        ['before-'+i for i in rename_duplicates(bstep)] +
        # 为循环步添加'-CycleX'后缀，并处理重复名称。
        [ s + "-Cycle{}".format(i) for i in range(1, cyctimes+1) for s in rename_duplicates(csteplist) ] +
        # 为后置步添加'after-'前缀，并处理重复名称。
        ['after-'+i for i in rename_duplicates(astep)]
    )
    # 检查最终生成的分析步名称列表中是否存在重复项。
    if len(steplist) != len(set(steplist)):
        # 如果存在重复，则抛出异常。
        raise Exception('Duplicates in steplist!')
    # 返回一个zip对象，将分析步名称列表和时间周期列表配对。
    return zip(steplist,timelist)

# 定义一个函数，用于检查和确定分析步类型的兼容性。
def compatibleStepTypeCheck(m,existing_steps,modeltype):
    # 定义结构分析步的集合。
    STRUCTURAL_STEPS = {'StaticStep', 'ViscoStep'}
    # 定义热分析步的集合。
    THERMAL_STEPS = {'HeatTransferStep', 'CoupledThermalElectricStep'}
    # 自动确定分析步类型。
    if modeltype in (None, 'AUTO'):
        # 如果当前模型中没有现有分析步。
        if not existing_steps:
            # 设定默认模型类型为'StaticStep'（完全缺省情况）。
            modeltype = 'StaticStep'
        else:
            # 获取最后一个现有分析步的名称。
            last_step = m.steps[existing_steps[-1]]
            # 将模型类型设置为最后一个分析步的类型。
            modeltype = last_step.__name__
    else:
        # 手动指定模型类型时的类型校验。
        if existing_steps:
            # 获取最后一个现有分析步的名称。
            last_step = m.steps[existing_steps[-1]]
            # 获取最后一个分析步的类型名称。
            last_type = last_step.__name__

            # 确定允许的分析步类型集合，基于最后一个分析步的类型。
            if last_type in STRUCTURAL_STEPS:
                # 如果是结构步，则后续只能是结构步。
                allowed_types = STRUCTURAL_STEPS
            elif last_type in THERMAL_STEPS:
                # 如果是热分析步，则后续只能是热分析步。
                allowed_types = THERMAL_STEPS
            else:
                # 其他类型的分析步，不限制后续类型（或认为不兼容）。
                allowed_types = None
            # 类型冲突检测。
            # 如果存在允许的类型集合，并且手动指定的modeltype不在其中。
            if allowed_types and (modeltype not in allowed_types):
                # 抛出ValueError，提示分析步序列不兼容。
                raise ValueError(
                    "Invalid step sequence: {0} cannot follow {1}.\n"
                    "Allowed types after {1} are: {2}".format(
                        modeltype,
                        last_type,
                        ', '.join(allowed_types)
                    )
                )
    # 返回最终确定的分析步类型。
    return modeltype

# 定义主函数，用于构建或修改Abaqus模型中的分析步。
def pre_stepBuild(bstep, csteplist, steptimepair, astep, cyctimes, modeltype=None,creatFlag='REPLACE',elasticFlag=False,fortfilepath=''):
    # 初始化刷新标志，用于判断是否需要刷新MDB。
    refreshFlag=0
    # 获取当前Abaqus模型对象。
    m = get_current_model()
    # 获取模型中所有现有分析步的名称，排除"Initial"步。
    existing_steps = [name for name in m.steps.keys() if name != "Initial"]
    # 检查并确定分析步的兼容类型。
    modeltype=compatibleStepTypeCheck(m,existing_steps,modeltype)
    # 如果模型类型是传热步或设置了弹性标志，则将循环次数强制设为1。
    if modeltype=='HeatTransferStep' or elasticFlag:
        cyctimes=1
    # 调用fulllist函数，生成所有分析步的配置列表（名称和时间周期）。
    configs = list(fulllist(bstep, csteplist, steptimepair, astep, cyctimes))
    # 获取现有分析步的数量。
    n_existing = len(existing_steps)
    # 获取新配置中分析步的数量。
    n_configs = len(configs)
    # 根据创建标志（creatFlag）执行不同的操作。
    if creatFlag=='REPLACE':
        # 如果是替换模式，设置刷新标志为True。
        refreshFlag=True # 2025年6月19日测试
        # 计算需要忽略的分析步数量（前置步的数量）。
        ignoreStep=len(bstep)
        # 1. 先处理已有的步骤：依次对前 n_existing 个配置修改已有步骤。
        # 遍历现有分析步和新配置中较少的部分。
        for i in range(min(n_existing, n_configs)):
            # 获取旧的分析步名称。
            old_name = existing_steps[i]
            # 获取新的分析步名称和时间周期。
            new_name, timePeriod = configs[i]
            # 如果旧名称和新名称不同，表示需要重命名。
            if old_name != new_name:
                # 当第一步就需要改名时，需要刷新mdb。
                if i==0:
                    refreshFlag=True
                # 如果新名称已经存在于模型步骤中。
                if new_name in m.steps:
                    # 临时修改已存在的新名称，避免冲突。
                    m.steps.changeKey(fromName=new_name, toName=new_name+'_tmp')
                    # 更新existing_steps列表中的名称。
                    existing_steps[existing_steps.index(new_name)]=new_name+'_tmp'
                # 更新existing_steps列表中的旧名称为新名称。
                existing_steps[existing_steps.index(old_name)]=new_name
                # 执行Abaqus的重命名操作。
                m.steps.changeKey(fromName=old_name, toName=new_name)
                # print('step name {0} change to {1}'.format(old_name,new_name)) # 调试信息。
            # 获取创建分析步的方法和默认参数。
            method, paras = stepBuildMap(modeltype)
            # 更新参数字典，包括名称、前置步、时间周期、初始增量步和最大增量步。
            paras.update({
                'name': new_name,
                'previous': (['Initial']+existing_steps)[i], # 确定前置步。
                'timePeriod': timePeriod,
                # 'initialInc': timePeriod / 1000.0, # 初始增量步可以根据时间周期设置。
                'initialInc': 1.0, # 或者固定为1.0。
                'maxInc': timePeriod/ 20.0, # 最大增量步。
            })
            # 如果模型类型是粘弹性步或静态步，则开启几何非线性。
            if modeltype=='ViscoStep' or modeltype=='StaticStep':
                paras.update({'nlgeom':ON})
            # 如果当前分析步的类型与模型类型不符，则保留现有属性。
            if m.steps[new_name].__name__ !='modeltype':
                paras.update({'maintainAttributes':True,})
            # 调用Abaqus模型对象上的方法来创建或修改分析步。
            getattr(m, method)(**paras)
            # 如果是第一个分析步。
            if i==0:
                # 如果存在名为'F-Output-0'的场输出请求，则删除它。
                if 'F-Output-0' in m.fieldOutputRequests:
                    del m.fieldOutputRequests['F-Output-0']
                # 创建新的场输出请求。
                m.FieldOutputRequest(name='F-Output-0',
                                      createStepName=new_name, # 在当前分析步创建。
                                      variables=fieldOutputRequestsMap(m,modeltype)) # 设置输出变量。
            else:
                # 对于后续分析步，尝试设置其场输出变量。
                try:
                    m.fieldOutputRequests['F-Output-0'].setValuesInStep(stepName=new_name,
                        variables=fieldOutputRequestsMap(m,modeltype))
                except Exception as e:
                    # 捕获并打印异常信息，特别是当输出请求不存在或被抑制时。
                    if str(e)=='The output request does not exist in the specifed step or is suppressed or inactive.':
                        print('{e} : {s}'.format(e=str(e),s=new_name))
        # 2. 对于配置中超出的部分，创建新步骤。
        # 如果新配置的步数多于现有步数。
        if n_existing<n_configs:
            # 确定新步骤的前置步名称。
            # 如果有现有步骤，则取最后一个修改后的现有步骤名称；否则为"Initial"。
            previous = configs[n_existing - 1][0] if n_existing > 0 else "Initial"
            # 遍历新配置中超出现有步数的部分。
            for new_name, timePeriod in configs[n_existing:]:
                # 获取创建分析步的方法和默认参数。
                method, paras = stepBuildMap(modeltype)
                # 更新参数字典。
                paras.update({
                    'name': new_name,
                    'previous': previous,
                    'timePeriod': timePeriod,
                    # 'initialInc': timePeriod / 1000.0,
                    'initialInc': 1.0,
                    'maxInc': timePeriod / 20.0,
                    #'maintainAttributes':True, # 这行被注释掉了。
                })
                # 在创建分析步前，检查新名称是否已存在。
                if new_name in existing_steps:
                    # 如果存在，临时修改已存在的新名称。
                    m.steps.changeKey(fromName=new_name, toName=new_name+'__tmp__')
                    # 更新existing_steps列表中的名称。
                    existing_steps[existing_steps.index(new_name)]=new_name+'__tmp__'
                # 调用Abaqus模型对象上的方法来创建分析步。
                getattr(m, method)(**paras)
                # 更新前置步为当前创建的分析步名称。
                previous = new_name
                # 如果存在名为'F-Output-0'的场输出请求。
                if 'F-Output-0' in m.fieldOutputRequests:
                    # 尝试设置其场输出变量。
                    try:
                        m.fieldOutputRequests['F-Output-0'].setValuesInStep(stepName=new_name,
                            variables=fieldOutputRequestsMap(m,modeltype))
                    except Exception as e:
                        # 捕获并打印异常信息。
                        if str(e)=='The output request does not exist in the specifed step or is suppressed or inactive.':
                            print('{e} : {s}'.format(e=str(e),s=new_name))
        # 如果现有步数多于新配置的步数，则删除多余的分析步（实现替换效果）。
        elif n_existing>n_configs:
            # 从新配置的末尾开始，删除多余的现有分析步。
            for i in range(n_configs, n_existing):
                del m.steps[existing_steps[i]]
    # 如果创建标志是'NEW'（新增模式）。
    elif creatFlag=='NEW':
        # 计算需要忽略的分析步数量（现有步数 + 前置步的数量）。
        ignoreStep=n_existing+len(bstep)
        # 确定新步骤的前置步为模型中最后一个现有分析步。
        previous = m.steps.keys()[-1]
        # 遍历所有新配置的分析步。
        for new_name, timePeriod in configs:
            # 获取创建分析步的方法和默认参数。
            method, paras = stepBuildMap(modeltype)
            # 更新参数字典。
            paras.update({
                'name': new_name,
                'previous': previous,
                'timePeriod': timePeriod,
                # 'initialInc': timePeriod / 1000.0,
                'initialInc': 1.0,
                'maxInc': timePeriod/ 20.0,
                #'maintainAttributes':True,
            })
            # 在创建分析步前，检查新名称是否已存在。
            if new_name in existing_steps:
                # 如果存在，临时修改已存在的新名称。
                m.steps.changeKey(fromName=new_name, toName=new_name+'__tmp__')
                # 更新existing_steps列表中的名称。
                existing_steps[existing_steps.index(new_name)]=new_name+'__tmp__'
            # 调用Abaqus模型对象上的方法来创建分析步。
            getattr(m, method)(**paras)
            # 更新前置步为当前创建的分析步名称。
            previous = new_name
            # 如果存在名为'F-Output-0'的场输出请求。
            if 'F-Output-0' in m.fieldOutputRequests:
                # 尝试设置其场输出变量。
                try:
                    m.fieldOutputRequests['F-Output-0'].setValuesInStep(stepName=new_name,
                        variables=fieldOutputRequestsMap(m,modeltype))
                except Exception as e:
                    # 捕获并打印异常信息。
                    if str(e)=='The output request does not exist in the specifed step or is suppressed or inactive.':
                        print('{e} : {s}'.format(e=str(e),s=new_name))
            else:
                # 如果不存在'F-Output-0'，则创建新的场输出请求。
                m.FieldOutputRequest(name='F-Output-0',
                                      createStepName=new_name,
                                      variables=fieldOutputRequestsMap(m,modeltype))

    # 定义一个内部辅助函数，用于计算需要忽略的分析步数量。
    def _calc_ignore(mdb_model, first_cycle_tag):
        """
        统计 'Initial' 之后、第一次出现 first_cycle_tag 之前
        已存在多少个分析步。
        """
        # 获取模型中除'Initial'之外的所有分析步名称。
        steps = [s for s in mdb_model.steps.keys() if s != 'Initial']
        # 打印第一个循环步的标签，用于调试。
        print("first_cycle_tagis:", first_cycle_tag)
        # 使用next和enumerate来查找第一个循环步的索引。
        # idx=0,1,2,… 保证 0-based 计数，正好就是 ignoreStep。
        return next((idx      # 返回索引。
                    for idx, s in enumerate(steps) # 遍历步骤及其索引。
                    if first_cycle_tag in s), # 如果当前步骤名称包含第一个循环步的标签。
                    len(steps))   # 如果没找到，则忽略所有步骤（即所有步骤都在循环步之前）。

    # 计算实际需要忽略的分析步数量。
    ignoreStep      = _calc_ignore(m, csteplist[0])  # ← m 是当前模型。
    # 获取循环步的数量。
    NResetStep      = len(csteplist)
    # 获取最后一个循环步的时间周期，用于疲劳分析。
    Time1forFatigue = steptimepair[csteplist[-1]]

    # 调用fortran_editer函数，更新Fortran文件中的占位符。
    fortran_editer(ignoreStep, NResetStep, Time1forFatigue)
    # 如果刷新标志为True。
    if refreshFlag:
        # 调用refreshMdb函数，保存并重新加载MDB文件。
        refreshMdb(mdb.pathName,m.name)

# 定义一个函数，用于刷新Abaqus模型数据库（MDB）。
def refreshMdb(path,modelname):
    # 对于Abaqus CAE不刷新界面的Bug，建议保存再读取。
    # 打印提示信息，告知用户可能需要进行弹窗操作。
    print(u'请在弹出窗口执行操作（如未见弹窗可能被遮挡）'.encode('GB18030'))
    # 弹出警告对话框，询问用户是否保存并刷新CAE文件。
    flag=getWarningReply(u'保存并刷新CAE文件?'.encode('GB18030'), (YES,CANCEL))

    # 如果用户选择“是”（YES）。
    if flag==YES:
        # 保存当前MDB文件。
        mdb.save()
        # 重新打开MDB文件。
        openMdb(pathName=path)
        # 获取当前视口名称。
        viewport = session.currentViewportName
        # 将当前视口显示的对象设置为指定模型的根装配体。
        session.viewports[viewport].setValues(displayedObject=mdb.models[modelname].rootAssembly)
    else:
        # 如果用户选择“取消”，则不做任何操作。
        pass

# 定义一个函数，用于批量修改分析步的属性。
def pre_stepModify(stepnamelist,edittype,value):
    # 遍历传入的分析步名称列表。
    for step in stepnamelist:
        # 尝试修改每个分析步的属性。
        try:
            # 调用stepModifyMap函数进行修改。
            stepModifyMap(step,edittype,value)
        except Exception as e:
            # 如果修改过程中发生错误，则打印错误信息。
            print("Error in Modify step {st}: {er}".format(st=step,er=str(e)))

# 定义一个函数，用于编辑Fortran文件中的占位符。
def fortran_editer(ignoreStep, NResetStep, Time1forFatigue):
    """
    在插件根目录递归搜索所有 *.for（排除 fortranBase），
    将占位符一次性替换为运行时参数:

        {{ignoreStep}}      ->  int(ignoreStep)
        {{NResetStep}}      ->  int(NResetStep)
        {{Time1forFatigue}} ->  float(Time1forFatigue)
        {{MinCreepTemp}}    ->  371   (固定写死)

    Parameters
    ----------
    ignoreStep : int
    NResetStep : int
    Time1forFatigue : float
    """
    # 1. 插件根目录 = 当前脚本所在目录。
    # 获取当前脚本的绝对路径，并提取其目录。
    plugin_dir   = os.path.dirname(os.path.abspath(__file__))
    # 定义需要排除的Fortran基础模板目录路径。
    base_exclude = os.path.join(plugin_dir, 'fortranBase')

    # 2. 占位符 → 字符串映射。
    # 定义一个字典，键是正则表达式模式（匹配占位符），值是替换后的字符串。
    repl_map = {
        r'\{\{\s*ignoreStep\s*\}\}'      : str(int(ignoreStep)),      # 忽略步数。
        r'\{\{\s*NResetStep\s*\}\}'      : str(int(NResetStep)),      # 重置步数。
        r'\{\{\s*Time1forFatigue\s*\}\}' : str(float(Time1forFatigue)), # 疲劳分析的时间。
        r'\{\{\s*MinCreepTemp\s*\}\}'    : '371',                     # 最小蠕变温度，固定值。
    }

    # 3. 扫描并替换。
    # 遍历插件根目录及其子目录下的所有文件。
    for root, dirs, files in os.walk(plugin_dir):
        # 跳过模板目录，不处理其中的文件。
        if root.startswith(base_exclude):
            continue
        # 遍历当前目录下的所有文件。
        for fn in files:
            # 检查文件是否以'.for'（不区分大小写）结尾。
            if fn.lower().endswith('.for'):
                # 构建文件的完整路径。
                fpath = os.path.join(root, fn)

                # 以只读模式打开文件。
                with open(fpath, 'r') as f:
                    # 读取文件所有内容。
                    txt = f.read()

                # 初始化新文本为原始文本。
                new_txt = txt
                # 遍历替换映射字典。
                for pat, rep in repl_map.items():
                    # 使用正则表达式替换占位符，re.I表示不区分大小写。
                    new_txt = re.sub(pat, rep, new_txt, flags=re.I)

                # 如果新文本与原始文本不同，说明发生了替换。
                if new_txt != txt:
                    # 只有修改才写回文件。
                    with open(fpath, 'w') as f:
                        # 将修改后的内容写入文件。
                        f.write(new_txt)
                    # 打印更新信息。
                    print(u'[fortran_editer] 已更新: {}'.format(fpath).encode('gb18030'))


# 定义一个函数，用于修改Abaqus模型中的分析步属性
def stepModifyMap(stepname,edittype,value):
    # 获取当前Abaqus模型对象
    m=get_current_model()
    # 根据分析步名称获取对应的分析步对象
    s=m.steps[stepname]
    # 根据编辑类型（edittype）执行不同的修改操作
    # 如果编辑类型是“设置时间周期”
    if edittype=='set Time period':
        # 获取分析步的setValues方法，用于修改分析步属性
        method=getattr(s,'setValues')
        # 定义要设置的参数字典
        paras={
            'timePeriod':value, # 设置分析步的总时间周期
            # 'initialInc': timePeriod / 1000.0, # 原始注释掉的行，可能用于计算初始增量
            'initialInc': 1.0, # 设置初始增量大小为1.0
            'maxInc': value/ 20.0, # 设置最大增量大小为总时间周期的1/20
            }
    # 如果编辑类型是“设置初始增量大小”
    elif edittype=='set Initial increment size':
        # 获取分析步的setValues方法
        method=getattr(s,'setValues')
        # 定义要设置的参数字典
        paras={
            'initialInc':value, # 设置初始增量大小为指定值
            }
    # 如果编辑类型是“设置最大增量大小”
    elif edittype=='set Max increment size':
        # 获取分析步的setValues方法
        method=getattr(s,'setValues')
        # 定义要设置的参数字典
        paras={
            'maxInc':value, # 设置最大增量大小为指定值
            }
    # 如果编辑类型是“设置在分析步内线性加载”
    elif edittype=='set Ramp linearly over step':
        # 获取分析步的setValues方法
        method=getattr(s,'setValues')
        # 定义要设置的参数字典
        paras={
            'amplitude':RAMP, # 设置幅值曲线为线性渐变（RAMP）
            }
    # 如果编辑类型是“设置矩阵存储为非对称”
    elif edittype=='set matrixStorage UNSYMMETRIC':
        # 获取分析步的setValues方法
        method=getattr(s,'setValues')
        # 定义要设置的参数字典
        paras={
            'matrixSolver':DIRECT, # 设置矩阵求解器为直接法
            'matrixStorage':UNSYMMETRIC, # 设置矩阵存储方式为非对称
            }
    # 如果编辑类型是“设置瞬时加载”
    elif edittype=='set Instantaneous':
        # 获取分析步的setValues方法
        method=getattr(s,'setValues')
        # 定义要设置的参数字典
        paras={
            'amplitude':STEP, # 设置幅值曲线为阶跃（STEP），即瞬时加载
            }
    # 如果编辑类型是“将Static分析步转换为Visco分析步”
    elif edittype=='change Static to Visco':
        # 检查当前分析步是否为StaticStep，如果不是则抛出异常
        if s.__name__!='StaticStep':
            # 抛出异常，提示分析步类型不匹配
            raise Exception("step '{s}' is {st}, no need/able to change to Visco".format(s=stepname,st=s.__name__))
        # 获取模型对象的ViscoStep方法，用于创建粘弹性分析步
        method=getattr(m,'ViscoStep')
        # 定义创建ViscoStep所需的参数，并从原StaticStep继承大部分属性
        paras={
            'cetol':0.05, # 粘弹性分析步的收敛容差
            'name':stepname, # 新分析步的名称，保持不变
            'previous':s.previous, # 前一个分析步的名称
            'timePeriod':s.timePeriod, # 时间周期
            'initialInc':s.initialInc, # 初始增量
            'maxNumInc':s.maxNumInc, # 最大增量步数
            'minInc':s.minInc, # 最小增量
            'maxInc':s.maxInc, # 最大增量
            'description':s.description, # 描述
            'convertSDI':s.convertSDI, # 是否转换SDI
            'matrixStorage':s.matrixStorage, # 矩阵存储方式
            'maintainAttributes':True, # 保持现有属性
            }
    # 如果编辑类型是“将Visco分析步转换为Static分析步”
    elif edittype=='change Visco to Static':
        # 检查当前分析步是否为ViscoStep，如果不是则抛出异常
        if s.__name__!='ViscoStep':
            # 抛出异常，提示分析步类型不匹配
            raise Exception("step '{s}' is {st}, no need/able to change to Static".format(s=stepname,st=s.__name__))
        # 获取模型对象的StaticStep方法，用于创建静力分析步
        method=getattr(m,'StaticStep')
        # 定义创建StaticStep所需的参数，并从原ViscoStep继承大部分属性
        paras={
            'name':stepname, # 新分析步的名称，保持不变
            'previous':s.previous, # 前一个分析步的名称
            'timePeriod':s.timePeriod, # 时间周期
            'initialInc':s.initialInc, # 初始增量
            'maxNumInc':s.maxNumInc, # 最大增量步数
            'minInc':s.minInc, # 最小增量
            'maxInc':s.maxInc, # 最大增量
            'description':s.description, # 描述
            'convertSDI':s.convertSDI, # 是否转换SDI
            'matrixStorage':s.matrixStorage, # 矩阵存储方式
            'maintainAttributes':True, # 保持现有属性
            }
    # 如果编辑类型是“启用重启功能”
    elif edittype=='enable Restart':
        # 获取分析步的Restart方法，用于设置重启请求
        method=getattr(s,'Restart')
        # 定义要设置的重启参数字典
        paras={
            'frequency':value, # 重启写入频率
            'numberIntervals':0, # 间隔数，0表示按频率
            'overlay':OFF, # 是否覆盖现有重启文件
            'timeMarks':OFF, # 是否在指定时间点写入重启文件
            }
    # 如果编辑类型不被识别
    else:
        # 打印提示信息，要求检查输入
        print('check input for {ed}'.format(ed=edittype))
    # 调用获取到的方法，并传入参数字典，执行分析步修改操作
    return method(**paras)
    
# 定义一个函数，用于根据模型类型构建分析步的创建参数
def stepBuildMap(modeltype):
    # 初始化一个默认参数字典，包含最大增量步数和最小增量
    paras={'maxNumInc': 1000000, # 最大增量步数
           'minInc':1E-9 # 最小增量
           }
    # 根据模型类型设置不同的创建方法和额外参数
    # 如果模型类型是“HeatTransferStep”（传热分析步）
    if modeltype=='HeatTransferStep':
        # 设置创建方法为'HeatTransferStep'
        method='HeatTransferStep'
        # 更新参数字典，添加传热分析步特有的参数
        paras.update({
            'deltmx':100.0, # 最大允许温度变化
            })
    # 如果模型类型是“ViscoStep”（粘弹性分析步）
    elif modeltype=='ViscoStep':
        # 设置创建方法为'ViscoStep'
        method='ViscoStep'
        # 更新参数字典，添加粘弹性分析步特有的参数
        paras.update({
            'cetol':0.05, # 粘弹性分析步的收敛容差
            })
    # 如果模型类型是“StaticStep”（静力分析步）
    elif modeltype=='StaticStep':
        # 设置创建方法为'StaticStep'
        method='StaticStep'
        # 静力分析步没有额外的特定参数，但仍更新以保持结构一致
        paras.update({
            })
    # 如果模型类型不被支持
    else:
        # 设置方法为传入的模型类型字符串
        method=modeltype
        # 抛出异常，提示该分析步类型暂不支持
        raise Exception('this step type {me} is not supported yet'.format(me=modeltype))   
    # 返回创建方法和对应的参数字典
    return (method,paras)

# 导入Counter和defaultdict，用于处理列表中的重复元素
from collections import Counter, defaultdict

# 定义一个函数，用于重命名列表中重复的元素，添加后缀
def rename_duplicates(lst):
    # 统计每个元素的总出现次数
    freq = Counter(lst)
    # 用于记录每个重复元素目前出现的次数
    occurrence = defaultdict(int)
    # 初始化结果列表
    result = []
    # 遍历输入列表中的每个元素
    for item in lst:
        # 如果元素出现多次（即频率大于1），则添加后缀
        if freq[item] > 1:
            # 增加该元素出现的次数计数
            occurrence[item] += 1
            # 将元素和其出现次数作为后缀添加到结果列表中
            result.append("{}-{}".format(item, occurrence[item]))
        # 如果元素只出现一次，则直接添加到结果列表中
        else:
            result.append(item)
    # 返回处理后的列表
    return result

# 当脚本作为主程序运行时执行以下代码块
if __name__ == '__main__':
    # 创建示例数据
    # bstep列表，可能用于存储构建的分析步信息
    bstep = []
    # csteplist，一个包含分析步名称的列表，可能包含重复项
    csteplist = ['G5','Steady','G6','Steady','G13','Steady','G9']
    # steptimepair，一个字典，映射分析步名称到其对应的时间周期
    steptimepair = {
        'G5':107600.0, # G5分析步的时间周期
        'Steady':480000.0, # Steady分析步的时间周期
        'G6':3000.0, # G6分析步的时间周期
        'G13':4500.0, # G13分析步的时间周期
        'G9':18000.0, # G9分析步的时间周期
        'HOLDING':20.0*365*24*3600, # HOLDING分析步的时间周期（20年）
        '*':1.0, # 缺省值，用于未明确指定时间周期的分析步
    }
    # astep列表，可能用于指定某些特定的分析步
    astep = ['G5','HOLDING']
    # 循环次数
    cyctimes = 5
    # modeltype，指定模型类型，这里被注释掉的是'ViscoStep'
    # modeltype = 'ViscoStep'
    # 当前设置为'AUTO'，表示自动选择模型类型
    modeltype = 'AUTO'
    # 创建标志，'REPLACE'可能表示替换现有分析步
    creatFlag='REPLACE'
    # 调用pre_stepBuild函数（此函数未在此代码段中定义，但被调用），用于预构建分析步
    pre_stepBuild(bstep, csteplist, steptimepair, astep, cyctimes, modeltype,creatFlag)
    
    # 以下是修改分析步的示例代码，当前被注释掉
    # 修改示例
    # edittype='change Static to Visco' # 编辑类型：将Static步改为Visco步
    # edittype='change Static to Visco' # 再次定义编辑类型，可能用于测试
    # value=0.05 # 对应编辑类型的值，例如cetol
    # stepnamelist=['G5-Cycle2','G5-Cycle3','Steady-1-Cycle2'] # 要修改的分析步名称列表
    # pre_stepModify(stepnamelist,edittype,value) # 调用pre_stepModify函数（此函数未在此代码段中定义）
