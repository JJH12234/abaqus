# -*- coding: utf-8 -*-
"""
Created on Fri Mar  7 02:00:24 2025

@author: mrvoid
"""
import os, glob, re
from abaqus import *
from abaqusConstants import *
from collections import Counter, defaultdict

def get_current_model():
    """获取当前视口关联的模型"""
    flag=None
    viewport = session.currentViewportName
    modelname=session.sessionState[viewport]['modelName']
    if 'Model-0' in modelname:
        print(u'请在弹出窗口执行操作（如未见弹窗可能被遮挡）'.encode('GB18030'))
        flag=getWarningReply(
            u'警告：不建议用户自行编辑Model-0 \n YES以继续（可能意外导致崩溃）; No以复制;'.encode('GB18030'), (YES,NO,CANCEL))
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
        # return mdb.models[modelname]
    else:
        pass
    return mdb.models[modelname]
def fulllist(bstep, csteplist, steptimepair, astep, cyctimes):
    all_steps=bstep+csteplist*cyctimes+astep
    timelist = [steptimepair[x] if x in steptimepair else steptimepair['*'] for x in all_steps]
    steplist = (
        ['before-'+i for i in rename_duplicates(bstep)] +
        [ s + "-Cycle{}".format(i) for i in range(1, cyctimes+1) for s in rename_duplicates(csteplist) ] +
        ['after-'+i for i in rename_duplicates(astep)]
    )
    if len(steplist) != len(set(steplist)):
        raise Exception('Duplicates in steplist!')
    return zip(steplist,timelist)

def compatibleStepTypeCheck(m,existing_steps,modeltype):
    # 定义分析步类型分组
    STRUCTURAL_STEPS = {'StaticStep', 'ViscoStep'}
    THERMAL_STEPS = {'HeatTransferStep', 'CoupledThermalElectricStep'}
    # 自动确定分析步类型
    if modeltype in (None, 'AUTO'):
        if not existing_steps:
            modeltype = 'StaticStep'  # 完全缺省情况
        else:
            last_step = m.steps[existing_steps[-1]]
            modeltype = last_step.__name__
    else:
        # 手动指定时的类型校验
        if existing_steps:
            last_step = m.steps[existing_steps[-1]]
            last_type = last_step.__name__
            
            # 确定允许的分析步类型集合
            if last_type in STRUCTURAL_STEPS:
                allowed_types = STRUCTURAL_STEPS
            elif last_type in THERMAL_STEPS:
                allowed_types = THERMAL_STEPS
            else:
                allowed_types = None  # 其他类型不限制
            # 类型冲突检测
            if allowed_types and (modeltype not in allowed_types):
                raise ValueError(
                    "Invalid step sequence: {0} cannot follow {1}.\n"
                    "Allowed types after {1} are: {2}".format(
                        modeltype, 
                        last_type,
                        ', '.join(allowed_types)
                    )
                )
    return modeltype  # 返回最终确定的分析步类型

def pre_stepBuild(bstep, csteplist, steptimepair, astep, cyctimes, modeltype=None,creatFlag='REPLACE',elasticFlag=False,fortfilepath=''):
    refreshFlag=0
    m = get_current_model()
    existing_steps = [name for name in m.steps.keys() if name != "Initial"] # 过滤掉 "Initial" 步，假设 "Initial" 为模型初始步，不修改
    modeltype=compatibleStepTypeCheck(m,existing_steps,modeltype)
    if modeltype=='HeatTransferStep' or elasticFlag:
        cyctimes=1
    # fulllist 返回一个列表，每个元素是 (新名称, timePeriod)
    configs = list(fulllist(bstep, csteplist, steptimepair, astep, cyctimes))
    n_existing = len(existing_steps)
    n_configs = len(configs)
    if creatFlag=='REPLACE':
        refreshFlag=True#2025年6月19日测试
        ignoreStep=len(bstep)
        # 1. 先处理已有的步骤：依次对前 n_existing 个配置修改已有步骤
        for i in range(min(n_existing, n_configs)):
            old_name = existing_steps[i]
            new_name, timePeriod = configs[i]
            if old_name != new_name:#当需要重命名既有分析步时
                if i==0:#当第一步就要改名时需要刷新mdb
                    refreshFlag=True
                if new_name in m.steps: #改名前先检查
                    m.steps.changeKey(fromName=new_name, toName=new_name+'_tmp')
                    existing_steps[existing_steps.index(new_name)]=new_name+'_tmp'
                existing_steps[existing_steps.index(old_name)]=new_name
                m.steps.changeKey(fromName=old_name, toName=new_name)
                # print('step name {0} change to {1}'.format(old_name,new_name))
            method, paras = stepBuildMap(modeltype)
            paras.update({
                'name': new_name,
                'previous': (['Initial']+existing_steps)[i],
                'timePeriod': timePeriod,
                # 'initialInc': timePeriod / 1000.0,
                'initialInc': 1.0,
                'maxInc': timePeriod/ 20.0,
            })
            if modeltype=='ViscoStep' or modeltype=='StaticStep':
                paras.update({'nlgeom':ON})
            if m.steps[new_name].__name__ !='modeltype': #类型切换时增加保留属性参数
                paras.update({'maintainAttributes':True,})
            getattr(m, method)(**paras)
            if i==0:
                if 'F-Output-0' in m.fieldOutputRequests:
                    del m.fieldOutputRequests['F-Output-0']#删除重名场输出
                m.FieldOutputRequest(name='F-Output-0',#再创建场输出
                                      createStepName=new_name,
                                      variables=fieldOutputRequestsMap(m,modeltype))
            else:
                try:
                    m.fieldOutputRequests['F-Output-0'].setValuesInStep(stepName=new_name,
                        variables=fieldOutputRequestsMap(m,modeltype))
                except Exception as e:
                    if str(e)=='The output request does not exist in the specifed step or is suppressed or inactive.':
                        print('{e} : {s}'.format(e=str(e),s=new_name))
        # 2. 对于配置中超出的部分，创建新步骤
        # 如果已有步骤存在，则新步骤的 previous 取最后修改后的步骤名称，否则为 "Initial"
        if n_existing<n_configs:
            previous = configs[n_existing - 1][0] if n_existing > 0 else "Initial"
            for new_name, timePeriod in configs[n_existing:]:
                method, paras = stepBuildMap(modeltype)
                paras.update({
                    'name': new_name,
                    'previous': previous,
                    'timePeriod': timePeriod,
                    # 'initialInc': timePeriod / 1000.0,
                    'initialInc': 1.0,
                    'maxInc': timePeriod / 20.0,
                    #'maintainAttributes':True,
                })
                if new_name in existing_steps: #创建分析步前先检查
                    m.steps.changeKey(fromName=new_name, toName=new_name+'__tmp__')
                    existing_steps[existing_steps.index(new_name)]=new_name+'__tmp__'
                getattr(m, method)(**paras)
                previous = new_name
                if 'F-Output-0' in m.fieldOutputRequests:
                    try:
                        m.fieldOutputRequests['F-Output-0'].setValuesInStep(stepName=new_name,
                            variables=fieldOutputRequestsMap(m,modeltype))
                    except Exception as e:
                        if str(e)=='The output request does not exist in the specifed step or is suppressed or inactive.':
                            print('{e} : {s}'.format(e=str(e),s=new_name))
        elif n_existing>n_configs: #删除多余分析步(以实现替换分析步列表的效果)
            for i in range(n_configs, n_existing):
                del m.steps[existing_steps[i]]
    elif creatFlag=='NEW':
        ignoreStep=n_existing+len(bstep)
        previous = m.steps.keys()[-1]
        for new_name, timePeriod in configs:
            method, paras = stepBuildMap(modeltype)
            paras.update({
                'name': new_name,
                'previous': previous,
                'timePeriod': timePeriod,
                # 'initialInc': timePeriod / 1000.0,
                'initialInc': 1.0,
                'maxInc': timePeriod/ 20.0,
                #'maintainAttributes':True,
            })
            if new_name in existing_steps: #创建分析步前先检查
                m.steps.changeKey(fromName=new_name, toName=new_name+'__tmp__')
                existing_steps[existing_steps.index(new_name)]=new_name+'__tmp__'
            getattr(m, method)(**paras)
            previous = new_name
            if 'F-Output-0' in m.fieldOutputRequests:
                try:
                    m.fieldOutputRequests['F-Output-0'].setValuesInStep(stepName=new_name,
                        variables=fieldOutputRequestsMap(m,modeltype))
                except Exception as e:
                    if str(e)=='The output request does not exist in the specifed step or is suppressed or inactive.':
                        print('{e} : {s}'.format(e=str(e),s=new_name))
            else:
                m.FieldOutputRequest(name='F-Output-0',#再创建场输出
                                      createStepName=new_name,
                                      variables=fieldOutputRequestsMap(m,modeltype))
    def _calc_ignore(mdb_model, first_cycle_tag):
        """
        统计 'Initial' 之后、第一次出现 first_cycle_tag 之前
        已存在多少个分析步。
        """
        steps = [s for s in mdb_model.steps.keys() if s != 'Initial']
        print("first_cycle_tagis:", first_cycle_tag)
        # enumerate 保证 0-based 计数，正好就是 ignoreStep
        return next((idx      # idx=0,1,2,…
                    for idx, s in enumerate(steps)
                    if first_cycle_tag in s),
                    len(steps))   # 没找到就全部忽略

    ignoreStep      = _calc_ignore(m, csteplist[0])  # ← m 是当前模型
    NResetStep      = len(csteplist)
    Time1forFatigue = steptimepair[csteplist[-1]]

    fortran_editer(ignoreStep, NResetStep, Time1forFatigue)
    if refreshFlag:
        refreshMdb(mdb.pathName,m.name)
def refreshMdb(path,modelname):
    #对于abaqusCAE不刷新Bug，建议保存再读取
    print(u'请在弹出窗口执行操作（如未见弹窗可能被遮挡）'.encode('GB18030'))
    flag=getWarningReply(u'保存并刷新CAE文件?'.encode('GB18030'), (YES,CANCEL))

    if flag==YES:
        mdb.save()
        openMdb(pathName=path)
        viewport = session.currentViewportName
        session.viewports[viewport].setValues(displayedObject=mdb.models[modelname].rootAssembly)
    else:
        pass

def pre_stepModify(stepnamelist,edittype,value):
    for step in stepnamelist:
        try:
            stepModifyMap(step,edittype,value)
        except Exception as e:
            print("Error in Modify step {st}: {er}".format(st=step,er=str(e)))

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
    # 1. 插件根目录 = 当前脚本所在目录
    plugin_dir   = os.path.dirname(os.path.abspath(__file__))
    base_exclude = os.path.join(plugin_dir, 'fortranBase')

    # 2. 占位符 → 字符串映射
    repl_map = {
        r'\{\{\s*ignoreStep\s*\}\}'      : str(int(ignoreStep)),
        r'\{\{\s*NResetStep\s*\}\}'      : str(int(NResetStep)),
        r'\{\{\s*Time1forFatigue\s*\}\}' : str(float(Time1forFatigue)),
        r'\{\{\s*MinCreepTemp\s*\}\}'    : '371',
    }

    # 3. 扫描并替换
    for root, dirs, files in os.walk(plugin_dir):
        # 跳过模板目录
        if root.startswith(base_exclude):
            continue
        for fn in files:
            if fn.lower().endswith('.for'):
                fpath = os.path.join(root, fn)

                with open(fpath, 'r') as f:
                    txt = f.read()

                new_txt = txt
                for pat, rep in repl_map.items():
                    new_txt = re.sub(pat, rep, new_txt, flags=re.I)

                if new_txt != txt:            # 只有修改才写回
                    with open(fpath, 'w') as f:
                        f.write(new_txt)
                    print(u'[fortran_editer] 已更新: {}'.format(fpath).encode('gb18030'))

def fieldOutputRequestsMap(m,modeltype):
    variables=[]
    for mname in m.materials.keys():
        mm=m.materials[mname]
        if getattr(mm,'userOutputVariables',None):
            variables.append('UVARM')
            break
    for mname in m.materials.keys():
        mm=m.materials[mname]
        if getattr(mm,'userDefinedField',None):
            variables.append('STATUS')
            break
    if modeltype=='ViscoStep':
        variables.extend([
            'S', 'TRIAX', 'E', 'PE', 'PEEQ', 'PEMAG', 'IE', 'CE', 'CEEQ', 'CEMAG', 
            'THE', 'LE', 'U', 'RF', 'CF', 'CSTRESS', 'CDISP', 'NT', 'TEMP', 'COORD', ]
            )
        return tuple(variables)
    elif modeltype=='StaticStep':
        variables.extend([
            'S', 'TRIAX', 'E', 'PE', 'PEEQ', 'PEMAG', 'IE', 
            'THE', 'LE', 'U', 'RF', 'CF', 'CSTRESS', 'CDISP', 'NT', 'TEMP', 'COORD', ]
            )
        return tuple(variables)
    elif modeltype=='HeatTransferStep':
        variables.extend([
            'NT','TEMP','HFL','RFL', ]
            )
        return tuple(variables)
    else:
        variables=PRESELECT
        return variables

def stepModifyMap(stepname,edittype,value):
    m=get_current_model()
    s=m.steps[stepname]
    if edittype=='set Time period':
        method=getattr(s,'setValues')
        paras={
            'timePeriod':value,
            # 'initialInc': timePeriod / 1000.0,
            'initialInc': 1.0,
            'maxInc': value/ 20.0,
            }
    elif edittype=='set Initial increment size':
        method=getattr(s,'setValues')
        paras={
            'initialInc':value,
            }
    elif edittype=='set Max increment size':
        method=getattr(s,'setValues')
        paras={
            'maxInc':value,
            }
    elif edittype=='set Ramp linearly over step':
        method=getattr(s,'setValues')
        paras={
            'amplitude':RAMP,
            }
    elif edittype=='set matrixStorage UNSYMMETRIC':
        method=getattr(s,'setValues')
        paras={
            'matrixSolver':DIRECT,
            'matrixStorage':UNSYMMETRIC,
            }
    elif edittype=='set Instantaneous':
        method=getattr(s,'setValues')
        paras={
            'amplitude':STEP,
            }
    elif edittype=='change Static to Visco':
        if s.__name__!='StaticStep':
            raise Exception("step '{s}' is {st}, no need/able to change to Visco".format(s=stepname,st=s.__name__))
        method=getattr(m,'ViscoStep')
        paras={
            'cetol':0.05,
            'name':stepname,
            'previous':s.previous,
            'timePeriod':s.timePeriod,
            'initialInc':s.initialInc,
            'maxNumInc':s.maxNumInc,
            'minInc':s.minInc,
            'maxInc':s.maxInc,
            'description':s.description,
            'convertSDI':s.convertSDI,
            'matrixStorage':s.matrixStorage,
            'maintainAttributes':True,
            }
    elif edittype=='change Visco to Static':
        if s.__name__!='ViscoStep':
            raise Exception("step '{s}' is {st}, no need/able to change to Static".format(s=stepname,st=s.__name__))
        method=getattr(m,'StaticStep')
        paras={
            'name':stepname,
            'previous':s.previous,
            'timePeriod':s.timePeriod,
            'initialInc':s.initialInc,
            'maxNumInc':s.maxNumInc,
            'minInc':s.minInc,
            'maxInc':s.maxInc,
            'description':s.description,
            'convertSDI':s.convertSDI,
            'matrixStorage':s.matrixStorage,
            'maintainAttributes':True,
            }
    elif edittype=='enable Restart':
        method=getattr(s,'Restart')
        paras={
            'frequency':999999, 
            'numberIntervals':0,
            'overlay':OFF,
            'timeMarks':OFF,
            }
    else:
        print('check input for {ed}'.format(ed=edittype))
    return method(**paras)
    
def stepBuildMap(modeltype):
    paras={'maxNumInc': 1000000,
           'minInc':1E-9
           }
    if modeltype=='HeatTransferStep':
        method='HeatTransferStep'
        paras.update({
            'deltmx':100.0,
            })
    elif modeltype=='ViscoStep':
        method='ViscoStep'
        paras.update({
            'cetol':0.05,
            })
    elif modeltype=='StaticStep':
        method='StaticStep'
        paras.update({
            })
    else:
        method=modeltype
        raise Exception('this step type {me} is not supported yet'.format(me=modeltype))   
    return (method,paras)

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

if __name__ == '__main__':
    # 创建示例
    bstep = []
    csteplist = ['G5','Steady','G6','Steady','G13','Steady','G9']
    steptimepair = {
        'G5':107600.0,
        'Steady':480000.0,
        'G6':3000.0,
        'G13':4500.0,
        'G9':18000.0,
        'HOLDING':20.0*365*24*3600,
        '*':1.0, #缺省值
    }
    astep = ['G5','HOLDING']
    cyctimes = 5
    # modeltype = 'ViscoStep'
    modeltype = 'AUTO'
    creatFlag='REPLACE'
    pre_stepBuild(bstep, csteplist, steptimepair, astep, cyctimes, modeltype,creatFlag)
    
    # 修改示例
    # edittype='change Static to Visco'
    # edittype='change Static to Visco'
    # value=0.05
    # stepnamelist=['G5-Cycle2','G5-Cycle3','Steady-1-Cycle2']
    # pre_stepModify(stepnamelist,edittype,value)