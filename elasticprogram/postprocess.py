# -*- coding: utf-8 -*-
"""
Created on Thu Mar 13 13:15:45 2025

@author: mrvoid
"""
from abaqus import session
from abaqus import VisError, OdpError
from abaqusConstants import *
from collections import OrderedDict,defaultdict
import ast
import json
import numpy as np
import csv
# import brittle_assess
import datetime

def safe_del(name, container):
    "若存在同名对象则先删除"
    try:
        del container[name]
    except KeyError:
        pass                  
def kernal_analsys(analysetype,
           CreepDamageField,FatigueDamageField,CFICriterion,CreepDamageFieldnum,FatigueDamageFieldnum,
           pathStyle,numIntervals,shape,
           stepIDFs1,stepIDFs2,stepIDFs3,
           BrittleStress,
           extrapolateType,extrapolateTimes,addTypeStepNames,
           tabledata1=(),tabledata2=(),picks1=(),picks2=(),CREEP_WELD_FATOR=0.9,FATIGUE_WELD_FATOR=0.5):
    path_extras_configs={#用户希望路径输出
        'pathStyle':pathStyle, 
        'numIntervals':numIntervals, 
        'shape':shape, 
        }
    Field_configs={#变量位置
        'CreepDamage':CreepDamageField+str(CreepDamageFieldnum),
        'FatigueDamage':FatigueDamageField+str(FatigueDamageFieldnum),
        }
    Step_configs={
        'extrapolateTimes':extrapolateTimes, #外推次数
        'extrapolateType':extrapolateType, #Add
        'addTypeStepNames':addTypeStepNames.split(',') if addTypeStepNames else [],#增补分析步名称，暂不支持对路径IE增补
        'stepIDFs':[stepIDFs1,stepIDFs2,stepIDFs3], #循环前有几个分析步，每几步一个循环，最后几步不是循环
        'damageJudge':[float(x) for x in CFICriterion.split(',')], #疲劳，蠕变判据
        }
    if BrittleStress in ['S11','S22','S33','S12','S13','S23']:
        Brittle_variables = [
        {
            'name': 'S',
            'components': [BrittleStress]
        },]
    else:
        Brittle_variables = [
        {
            'name': 'S',
            'invariants': [BrittleStress]
        },]

    tabledata={'Points':tabledata1,'Paths':tabledata2}
    if analysetype==u'非弹性应变'.encode('GB18030'):
        kernel_IE(tabledata,Field_configs,Step_configs,kw1=picks1,kw2=picks2,path_extras_configs=path_extras_configs)
    elif analysetype==u'非弹性损伤'.encode('GB18030'):
        kernel_CreepFatigueDamage(tabledata,Field_configs,Step_configs,kw1=picks1,kw2=picks2,CREEP_WELD_FATOR=CREEP_WELD_FATOR,FATIGUE_WELD_FATOR=FATIGUE_WELD_FATOR)
    elif analysetype==u'防脆断分析'.encode('GB18030'):
        kernel_BrittleFailure(tabledata,Brittle_variables,path_extras_configs=path_extras_configs)
        # brittle_assess.run_gui()
        # execfile('C:/Users/mrvoid/abaqus_plugins/Inelasticprogram/brittle_assess.py',__main__.__dict__)
def kernel_CreepFatigueDamage(tabledata,Field_configs,Step_configs,kw1=(),kw2=(),path_extras_configs={},CREEP_WELD_FATOR=0.9,FATIGUE_WELD_FATOR=0.5):
    viewport=get_current_viewport()
    odbDisplay=get_current_odbdp()
    odb=get_current_odb()
    odbData=get_current_odbdata()
    #激活每循环最后分析步
    ActiveStepsFrames()
    if Step_configs['extrapolateType']=='Direct' or Step_configs['extrapolateType']=='None':
        ActiveStepsFrames(SSb='mod',SFb='last',bstep=Step_configs['stepIDFs'][0],cstep=Step_configs['stepIDFs'][1],astep=Step_configs['stepIDFs'][2])
    elif Step_configs['extrapolateType']=='Add':
        ActiveStepsFrames(SSb='mod and last',SFb='last',bstep=Step_configs['stepIDFs'][0],cstep=Step_configs['stepIDFs'][1],astep=Step_configs['stepIDFs'][2])
    #处理节点输入
    nodelabels, sortedflagged=process_points_data(tabledata['Points'],kw1,kw2)
    # print(sortedflagged)
    # print(sortedflagged)
    ##提取节点数据
    ###指定提取内容
    CreDamUV=Field_configs['CreepDamage']
    FatDamUV=Field_configs['FatigueDamage']
    variables=[{'name':CreDamUV ,},
               {'name':FatDamUV ,}]
    ###处理提取参数
    args=generate_xy_data_args(odb,'NODAL', variables, nodelabels)
    ###执行提取方法
    xylist=getxyData_point(args)
    ###定义字典记录节点数据
    Damages=OrderedDict()
    for xy in xylist:
        part,node=xy.yValuesLabel.split('at part instance ')[-1].split(' node ')
        node='node'+node
        field=xy.yValuesLabel.split(' ')[0]
        if part not in Damages:
            Damages[part]=OrderedDict()
        if node not in Damages[part]:
            Damages[part][node]=OrderedDict()
            Damages[part][node]['CreepDamageByCycle']=[]
            Damages[part][node]['FatigueDamageByCycle']=[]
            Damages[part][node]['judge']=None
        # print(node.split('node')[-1])
        # print(sortedflagged.get(part))
        # 转换节点编号为整数并安全获取列表
        node_num = int(node.split('node')[-1])
        if sortedflagged and node_num in sortedflagged.get(part, []):
            Damages[part][node]['isWeld']=True
        else:
            Damages[part][node]['isWeld']=False
        if field==CreDamUV:
            if Damages[part][node]['isWeld']==True:
                Damages[part][node]['CreepDamageByCycle']=[row[1]/CREEP_WELD_FATOR for row in xy.data]
            else:
                Damages[part][node]['CreepDamageByCycle']=[row[1] for row in xy.data]
        elif field==FatDamUV:
            if Damages[part][node]['isWeld']==True:
                Damages[part][node]['FatigueDamageByCycle']=[row[1]/FATIGUE_WELD_FATOR for row in xy.data]
            else:
                Damages[part][node]['FatigueDamageByCycle']=[row[1] for row in xy.data]
            if Step_configs['extrapolateType']=='Add':
                Damages[part][node]['FatigueDamageByCycle']=Damages[part][node]['FatigueDamageByCycle'][:-1]#去掉HOLDING点
    ###外推
    if Step_configs['extrapolateType']=='Direct':
        for part in Damages:
            for node in Damages[part]:
                Damages[part][node]['FatigueDamageByCycle']=directExtrapolate(Damages[part][node]['FatigueDamageByCycle'],Step_configs['extrapolateTimes'])
                Damages[part][node]['CreepDamageByCycle']=directExtrapolate(Damages[part][node]['CreepDamageByCycle'],Step_configs['extrapolateTimes'])
    elif Step_configs['extrapolateType']=='Add':
        for part in Damages:
            for node in Damages[part]:
                Damages[part][node]['FatigueDamageByCycle'].append(Damages[part][node]['FatigueDamageByCycle'][-1])
                Damages[part][node]['CreepDamageByCycle'].append(Damages[part][node]['CreepDamageByCycle'][-1])
        for stepname in Step_configs['addTypeStepNames']:
            # print(stepname)
            try:
                ActiveStepsFrames(SSb='Name',SFb='range',f_str='0,-1',name=stepname)
            except ValueError as e:
                e_unicode = unicode(str(e), 'gb18030', errors='replace')
                print(u"{},增补将跳过".format(e_unicode).encode('GB18030'))
                continue
            xylist=getxyData_point(args)
            for xy in xylist:
                part,node=xy.yValuesLabel.split('at part instance ')[-1].split(' node ')
                node='node'+node
                field=xy.yValuesLabel.split(' ')[0]
                if Damages[part][node]['isWeld']==True:
                    if field==CreDamUV:
                        delta=max(0,(xy.data[-1][1]-xy.data[0][1])/CREEP_WELD_FATOR)
                        Damages[part][node]['AddCreepDamage_'+stepname]=delta
                        Damages[part][node]['CreepDamageByCycle'][-1]+=delta*Step_configs['extrapolateTimes']
                    elif field==FatDamUV:
                        delta=max(0,(xy.data[-1][1]-xy.data[0][1])/FATIGUE_WELD_FATOR)
                        Damages[part][node]['AddFatigueDamage_'+stepname]=delta
                        Damages[part][node]['FatigueDamageByCycle'][-1]+=delta*Step_configs['extrapolateTimes']
                else:
                    if field==CreDamUV:
                        delta=max(0,(xy.data[-1][1]-xy.data[0][1]))
                        Damages[part][node]['AddCreepDamage_'+stepname]=max(0,delta)
                        Damages[part][node]['CreepDamageByCycle'][-1]+=delta*Step_configs['extrapolateTimes']
                    elif field==FatDamUV:
                        delta=max(0,(xy.data[-1][1]-xy.data[0][1]))
                        Damages[part][node]['AddFatigueDamage_'+stepname]=max(0,delta)
                        Damages[part][node]['FatigueDamageByCycle'][-1]+=delta*Step_configs['extrapolateTimes']
    for part in Damages:
        for node in Damages[part]:
            Damages[part][node]['judge']='Pass' if is_below_double_breakline(Damages[part][node]['FatigueDamageByCycle'][-1],Damages[part][node]['CreepDamageByCycle'][-1],Step_configs['damageJudge']) else 'NotPass'
    ###输出
    datetimenow=str(datetime.datetime.now()).split('.')[0].replace(':',"'")
    with open(r'Damage {}.json'.format(datetimenow), 'w') as f:
        json.dump(Damages, f, indent=4)
    print(u'Damage {}.json 已输出到工作路径'.format(datetimenow).encode('GB18030'))

def kernel_IE(tabledata,Field_configs,Step_configs,kw1=(),kw2=(),path_extras_configs={}):
    viewport=get_current_viewport()
    odbDisplay=get_current_odbdp()
    odb=get_current_odb()
    odbData=get_current_odbdata()
    #激活每循环最后分析步
    ActiveStepsFrames()
    if Step_configs['extrapolateType']=='Direct' or Step_configs['extrapolateType']=='None':
        ActiveStepsFrames(SSb='mod',SFb='last',bstep=Step_configs['stepIDFs'][0],cstep=Step_configs['stepIDFs'][1],astep=Step_configs['stepIDFs'][2])
    elif Step_configs['extrapolateType']=='Add':
        ActiveStepsFrames(SSb='mod and last',SFb='last',bstep=Step_configs['stepIDFs'][0],cstep=Step_configs['stepIDFs'][1],astep=Step_configs['stepIDFs'][2])
    #处理节点输入
    nodelabels, sortedflagged=process_points_data(tabledata['Points'],kw1,kw2)
    
    # print(sortedflagged)
    ##提取节点数据
    ###指定提取内容
    variables=[{'name':'PE' ,},{'name':'CE' ,},]
    ###处理提取参数
    args=generate_xy_data_args(odb,'NODAL', variables, nodelabels)
    ###执行提取方法
    xylist=getxyData_point(args)
    ###定义字典记录节点数据
    IE=OrderedDict()
    components=['11','22','33','12','13','23']
    for xy in xylist:
        part,node=xy.yValuesLabel.split('at part instance ')[-1].split(' node ')
        node='node'+node
        field=xy.yValuesLabel.split(' ')[0]
        if part not in IE:
            IE[part]=OrderedDict()
        if node not in IE[part]:
            IE[part][node]=OrderedDict()
            for pre in ['CE','PE']:
                for c in components:
                    IE[part][node][pre+c]=[]
        for component in components:
            for IEtype in ['PE','CE']:
                if field=='{}:{}{}'.format(IEtype,IEtype,component):
                    IE[part][node][IEtype+component]=[ row[1] for row in xy.data ]
    ###外推
    if Step_configs['extrapolateType']=='Direct':
        for part in IE:
            for node in IE[part]:
                for component in components:
                    if IE[part][node]['PE'+component] and IE[part][node]['CE'+component]:
                        #CE分量直接外推
                        IE[part][node]['CE'+component]=directExtrapolate(IE[part][node]['CE'+component],Step_configs['extrapolateTimes'])
                        if abs(max(IE[part][node]['PE'+component],key=abs))>abs(IE[part][node]['PE'+component][-1]):
                            #对于最后一步PE分量绝对值并非最大的情况，直接多次复制绝对值最大值
                            IE[part][node]['PE'+component]=IE[part][node]['PE'+component]+[max(IE[part][node]['PE'+component], key=abs)]*Step_configs['extrapolateTimes']
                        else:
                            #否则，直接外推
                            IE[part][node]['PE'+component]=directExtrapolate(IE[part][node]['PE'+component],Step_configs['extrapolateTimes'])
    elif Step_configs['extrapolateType']=='Add':
        for component in components:
            for part in IE:
                for node in IE[part]:
                    if IE[part][node]['PE'+component] and IE[part][node]['CE'+component]:
                        IE[part][node]['PE'+component].append(IE[part][node]['PE'+component][-1])
                        IE[part][node]['CE'+component].append(IE[part][node]['CE'+component][-1])
                        for stepname in Step_configs['addTypeStepNames']:
                            try:
                                ActiveStepsFrames(SSb='Name',SFb='range',f_str='0,-1',name=stepname)
                            except ValueError as e:
                                e_unicode = unicode(str(e), 'gb18030', errors='replace')
                                print(u"{},增补将跳过".format(e_unicode).encode('GB18030'))
                                continue
                            xylist=getxyData_point(args)
                            for xy in xylist:
                                part,node=xy.yValuesLabel.split('at part instance ')[-1].split(' node ')
                                node='node'+node
                                field=xy.yValuesLabel.split(' ')[0]
                                delta=xy.data[-1][1]-xy.data[0][1]
                                if field=='CE:CE{}'.format(component):
                                    IE[part][node]['CE'+component+'Add_'+stepname]=delta
                                    IE[part][node]['CE'+component][-1]+=delta*Step_configs['extrapolateTimes']
                                if field=='PE:PE{}'.format(component):
                                    IE[part][node]['PE'+component][-1] = max(IE[part][node]['PE'+component][:-1], key=abs)
                                    pass#等待考虑
    for part in IE:
        for node in IE[part]:
            CEs=[ IE[part][node]['CE'+c][-1] if IE[part][node]['CE'+c] else 0 for c in components]
            PEs=[ IE[part][node]['PE'+c][-1] if IE[part][node]['PE'+c] else 0 for c in components]
            IEs=[ CEs[i] + PEs[i] for i in range(len(CEs)) ]
            IE[part][node]['FinalIEmax']=max_principal_strain(*IEs)
            node_num=int(node.split('node')[-1])
            if sortedflagged!={} and (node_num in sortedflagged.get(part)):
                judge=0.025
                IE[part][node]['isWeld']=True
            else:
                judge=0.05
                IE[part][node]['isWeld']=False
            IE[part][node]['judge']='Pass' if IE[part][node]['FinalIEmax']<judge else 'NotPass >{}%'.format(str(judge*100))
    ###输出
    datetimenow=str(datetime.datetime.now()).split('.')[0].replace(':',"'")
    with open(r'IE_point {}.json'.format(datetimenow), 'w') as f:
        json.dump(IE, f, indent=4)
    print(u'IE_point {}.json 已输出到工作路径'.format(datetimenow).encode('GB18030'))
    
    #处理路径输入
    processd_path=process_path_data(tabledata['Paths'])
    ActiveStepsFrames()
    ###指定提取内容
    variables=[{'name':'PE' ,},{'name':'CE' ,},]
    #激活每循环最后分析步
    ActiveStepsFrames()
    if Step_configs['extrapolateType']=='Direct' or Step_configs['extrapolateType']=='None':
        ActiveStepsFrames(SSb='mod',SFb='last',bstep=Step_configs['stepIDFs'][0],cstep=Step_configs['stepIDFs'][1],astep=Step_configs['stepIDFs'][2])
    elif Step_configs['extrapolateType']=='Add':
        ActiveStepsFrames(SSb='mod and last',SFb='last',bstep=Step_configs['stepIDFs'][0],cstep=Step_configs['stepIDFs'][1],astep=Step_configs['stepIDFs'][2])
    # 创建分析步名称到索引的映射
    step_names = list(odbData.steps.keys())
    step_indices = {name: idx for idx, name in enumerate(step_names)}
    IE=OrderedDict()
    components=['11','22','33','12','13','23']
    for item in processd_path:
        # print(item)
        pthname,expression,flag=item
        IE[pthname]=OrderedDict()
        ###循环创建分析步
        pth=creatPath(pthname,expression)
        # 遍历每个激活的分析步及其增量步
        for step_info in odbData.activeFrames:
            step_name, frame_exprs = step_info
            step_idx = step_indices[step_name]  # 外循环的索引
            # 解析所有增量步表达式，合并为一个列表
            all_frame_indices = []
            for expr in frame_exprs:
                if isinstance(expr, str):
                    # 如果是字符串表达式（如'0:17:1'），调用 parse_range 解析
                    all_frame_indices.extend(parse_range(expr))
                else:
                    # 如果是直接给出的整数（如元组中的离散值），直接添加到列表
                    all_frame_indices.append(expr)
            # 内循环遍历每个激活的增量步索引
            for frame_idx in all_frame_indices:
                # 处理step_idx和frame_idx的组合
                XYname="{}__step{}__frame{}".format(pthname,step_idx,frame_idx)
                safe_del(XYname, session.xyDataObjects)
                args=generate_xypath_data_args(XYname,pth,step_idx,frame_idx,variables,extraconfigs=path_extras_configs)
                ###按步提取路径结果
                xylist=getxyData_path(args)
                for xy in xylist:
                    pthname=xy.name.split('__')[0]
                    varname=xy.name.split('-')[-1]
                    res=numpy_linear_regression(xy.data)
                    if varname in [e+c for e in ['PE','CE'] for c in components]:
                        if varname+'_ave' not in IE[pthname]:
                            IE[pthname][varname+'_ave']=[]
                        IE[pthname][varname+'_ave'].append(res[0])
                        if varname+'_p1' not in IE[pthname]:
                            IE[pthname][varname+'_p1']=[]
                        IE[pthname][varname+'_p1'].append(res[1])
                        if varname+'_p2' not in IE[pthname]:
                            IE[pthname][varname+'_p2']=[]
                        IE[pthname][varname+'_p2'].append(res[2])
    ###外推
    if Step_configs['extrapolateType']=='Direct':
        for pthname in IE:
            for component in components:
                for ptype in ['ave','p1','p2']:
                    if IE[pthname]['PE'+component+'_'+ptype] and IE[pthname]['CE'+component+'_'+ptype]:
                        IE[pthname]['CE'+component+'_'+ptype]=directExtrapolate(IE[pthname]['CE'+component+'_'+ptype],Step_configs['extrapolateTimes'])
                        if max(IE[pthname]['PE'+component+'_'+ptype])>IE[pthname]['PE'+component+'_'+ptype][-1]:
                            IE[pthname]['PE'+component+'_'+ptype]=IE[pthname]['PE'+component+'_'+ptype]+[max(IE[pthname]['PE'+component+'_'+ptype])]*Step_configs['extrapolateTimes']
                        else:
                            IE[pthname]['PE'+component+'_'+ptype]=directExtrapolate(IE[pthname]['PE'+component+'_'+ptype],Step_configs['extrapolateTimes'])
    elif Step_configs['extrapolateType']=='Add':
        pass
            
    for pthname in IE:
        for ptype in ['_ave','_p1','_p2']:
            CEs=[ IE[pthname]['CE'+c+ptype][-1] if IE[pthname]['CE'+c+ptype] else 0 for c in components]
            PEs=[ IE[pthname]['PE'+c+ptype][-1] if IE[pthname]['PE'+c+ptype] else 0 for c in components]
            IEs=[ CEs[i] + PEs[i] for i in range(len(CEs)) ]
            IE[pthname]['FinalIEmax'+ptype]=max_principal_strain(*IEs)
            for p in processd_path:
                if pthname==p[0]:
                    flag=processd_path[-1]
            if flag:
                IE[pthname]['isWeld']=True
            else:
                IE[pthname]['isWeld']=False
            if ptype=='_ave':
                judge=0.005 if flag else 0.01
            else:
                judge=0.01 if flag else 0.02
            IE[pthname]['judge'+ptype]='Pass' if IE[pthname]['FinalIEmax'+ptype]<judge else 'NotPass >{}%'.format(str(judge*100))
    ###输出
    datetimenow=str(datetime.datetime.now()).split('.')[0].replace(':',"'")
    with open(r'IE_path {}.json'.format(datetimenow), 'w') as f:
        json.dump(IE, f, indent=4)
    print(u'IE_path {}.json 已输出到工作路径'.format(datetimenow).encode('GB18030'))

def kernel_BrittleFailure(tabledata,user_variables,path_extras_configs={}):
    viewport=get_current_viewport()
    odbDisplay=get_current_odbdp()
    odb=get_current_odb()
    odbData=get_current_odbdata()
    ActiveStepsFrames()
    #处理路径输入
    processd_path=process_path_data(tabledata['Paths'])
    step_names = list(odbData.steps.keys())
    step_indices = {name: idx for idx, name in enumerate(step_names)}
    ###指定提取内容
    user_variables.append({'name': 'NT11','output_position':'NODAL',})
    datetimenow=str(datetime.datetime.now()).split('.')[0].replace(':',"'")
    for item in processd_path:
        pthname,expression,flag=item
        ###循环创建分析步
        pth=creatPath(pthname,expression)
        file_Sname='{}_{} {}.txt'.format(pthname,user_variables[0]['name'],datetimenow)
        file_Tname='{}_{} {}.txt'.format(pthname,user_variables[1]['name'],datetimenow)
        file_S = open(file_Sname, 'w')
        file_T = open(file_Tname, 'w')
        # 遍历每个激活的分析步及其增量步
        # 预先计算每个分析步的累积时间
        steps = list(odb.steps.values())
        # cumulative_times = [0.0]
        # for step in steps:
        #     cumulative_times.append(cumulative_times[-1] + step.totalTime)
        
        for step_info in odbData.activeFrames:
            step_name, frame_exprs = step_info
            step_idx = step_indices[step_name]  # 当前分析步索引
            # prev_total_time = cumulative_times[step_idx]  # 获取之前所有步的总时间
            
            # 解析所有增量步表达式，合并为一个列表
            all_frame_indices = []
            for expr in frame_exprs:
                if isinstance(expr, str):
                    all_frame_indices.extend(parse_range(expr))
                else:
                    all_frame_indices.append(expr)
            
            # 内循环遍历每个激活的增量步索引
            for counter,frame_idx in enumerate(all_frame_indices,1):
                # 当前frame在本分析步内的时间
                frame_time = odb.steps.values()[step_idx].frames[frame_idx].frameValue
                # 总时间 = 之前所有步总时间 + 当前frame时间
                total_time = odb.steps.values()[step_idx].totalTime + frame_time
                # total_time = prev_total_time + frame_time
                # 处理step_idx和frame_idx的组合
                XYname="{}__step{}__frame{}".format(pthname,step_idx,frame_idx)
                safe_del(XYname, session.xyDataObjects)
                args=generate_xypath_data_args(XYname,pth,step_idx,frame_idx,user_variables,extraconfigs=path_extras_configs)
                ###按步提取路径结果
                xylist=getxyData_path(args)
                for xy in xylist:
                    pthname=xy.name.split('__')[0]
                    varname=xy.name.split('-')[-1]
                    datas=[total_time]+[row[1] for row in xy.data]
                    if varname in ['Mises','Tresca','Max. Principal','Min. Principal','Mid. Principal','Max. Principal (Abs)','S11','S22','S33','S12','S13','S23','Pressure']:
                        file_S.write('\t'.join(map(str, datas)) + '\n')
                        # print(u'正在写入file_S {}:step:{} {}/{}'.format(pthname,step_name,str(counter),len(all_frame_indices)).encode('GB18030'))
                    # elif varname=='NT11':
                    else:
                        file_T.write('\t'.join(map(str, datas)) + '\n')    
                        # print(u'正在写入file_T {}:step:{} {}/{}'.format(pthname,step_name,str(counter),len(all_frame_indices)).encode('GB18030'))
        file_S.close()
        print(u'{} 已输出到工作路径'.format(file_Sname).encode('GB18030'))
        file_T.close()
        print(u'{} 已输出到工作路径'.format(file_Tname).encode('GB18030'))
    pass

def directExtrapolate(lst, n):
    # 参数校验
    if len(lst) < 2:
        raise ValueError("Input list must contain at least two elements")
    if n < 0:
        raise ValueError("Extension count n must be non-negative")
    # 计算基础差值
    base_diff = lst[-1] - lst[-2]
    last_value = lst[-1]
    # 直接生成扩展序列 (数学推导式)
    extension = [last_value + base_diff * (i+1) for i in xrange(n)]  # Python2.7使用xrange
    # 返回合并后的新序列
    return lst + extension

def is_below_double_breakline(x, y, c):
    """判断点(x,y)是否位于(0,1)-(a,b)-(1,0)构成的双折线下方"""
    a,b=c
    # 处理第一条线段（从(0,1)到(a,b)）
    if a != 0:
        x_min1, x_max1 = sorted((0, a))
        if x_min1 <= x <= x_max1:
            t = x / float(a)
            if 0 <= t <= 1:
                y_line = 1 + t * (b - 1)
                if y < y_line:
                    return True
    else:  # 垂直线x=0的特殊处理
        if x == 0:
            min_y = min(1, b)
            if y < min_y:
                return True

    # 处理第二条线段（从(a,b)到(1,0)）
    if a != 1:
        x_min2, x_max2 = sorted((a, 1))
        if x_min2 <= x <= x_max2:
            denominator = 1.0 - a
            t = (x - a) / denominator
            if 0 <= t <= 1:
                y_line = b * (1 - t)
                if y < y_line:
                    return True
    else:  # 垂直线x=1的特殊处理
        if x == 1:
            min_y = min(b, 0)
            if y < min_y:
                return True

    return False

def get_current_viewport():
    return session.viewports[session.currentViewportName]

def get_current_odbdp():
    return get_current_viewport().odbDisplay

def get_current_odb():
    return session.odbs[get_current_odbdp().name]

def get_current_odbdata():
    return session.odbData[get_current_odbdp().name]

def process_path_data(pathdata):
    def process_str_value(s):
        elements = s.split(',')
        processed = []
        for elem in elements:
            elem = elem.strip()  # 可选：清理前后空格
            if ':' in elem:
                processed.append(elem)
            else:
                processed.append(int(elem))  # 假设非冒号内容均可转为整型
        return tuple(processed)
    grouped_data = {}
    path_order = []
    for entry in pathdata:
        path, part, value_str, flag = entry
        processed_val = process_str_value(value_str)
        if path not in grouped_data:
            grouped_data[path] = {'entries': [], 'has_true': False}
            path_order.append(path)
        grouped_data[path]['entries'].append( (part, processed_val) )
        if flag:
            grouped_data[path]['has_true'] = True
    return tuple(
        (path, tuple(grouped_data[path]['entries']), grouped_data[path]['has_true'])
        for path in path_order)

# 定义一个函数，用于处理点数据。
# `pointdata` 预期是一个列表，其中每个元素是一个元组，包含 (部件名, 节点字符串, 标记)。
# `kw1` 和 `kw2` 是可选的关键字参数，用于处理额外的节点数据，默认为空元组。
def process_points_data(pointdata,kw1=(),kw2=()):
    # 初始化一个列表，用于存储所有处理后的节点标签。
    # 格式为 (部件实例名称, (节点表达式字符串,))。
    node_labels = []
    # 初始化一个 `defaultdict`，其默认值是 `set`。
    # 用于存储被标记的点的集合，键是部件名，值是该部件下所有被标记的点的唯一集合。
    flagged_points_dict = defaultdict(set)  # 使用集合自动去重
    # 遍历输入的 `pointdata` 列表中的每个条目。
    for item in pointdata:
        # 解包当前条目，获取部件名称、节点字符串和标记（布尔值）。
        part_name, nodes_str, flag = item
        # 分割节点表达式字符串（ 包含逗号分隔的多个表达式），并清理每个表达式两端的空格。
        # `if expr.strip()` 确保只处理非空表达式。
        node_exprs = [expr.strip() for expr in nodes_str.split(',') if expr.strip()]
        # 遍历每个节点表达式，构建节点参数结构。
        for expr in node_exprs:
            # 将 (部件名称, (节点表达式,)) 添加到 `node_labels` 列表中。
            node_labels.append((part_name, (expr,)))
        # 处理标记点：仅当 `flag` 为 True 时才执行此块。
        if flag:
            # 遍历当前条目中的每个节点表达式。
            for expr in node_exprs:
                # 使用 `parse_range` 函数解析节点表达式，将其转换为具体的整数点列表。
                points = parse_range(expr)
                # 如果 `parse_range` 返回了有效的点列表（非空）。
                if points:
                    # 将这些点添加到 `flagged_points_dict` 中对应部件的集合里。
                    # `update` 方法用于添加多个元素到集合中，并自动去重。
                    flagged_points_dict[part_name].update(points)
    # 处理kw参数的新函数
    def process_kw_nodes(kw_nodes, is_flagged=False):
        # 遍历 `kw_nodes` 中的每一个节点对象。
        for node in kw_nodes:
            # 获取节点的实例名称作为部件名。
            part_name = node.instanceName
            # 检查该部件实例是否存在于当前 ODB 的根装配中。
            if part_name in get_current_odb().rootAssembly.instances:
                # 获取节点的标签（通常是节点 ID 或范围），并清理空格。
                expr = str(node.label).strip()
                # 将 (部件名称, (节点标签,)) 添加到 `node_labels` 列表中。
                node_labels.append((part_name, (expr,)))
                # 如果 `is_flagged` 为 True，则处理标记点。
                if is_flagged:
                    # 解析节点标签，获取具体的点列表。
                    points = parse_range(expr)
                    # 如果解析成功并返回了点。
                    if points:
                        # 将这些点添加到 `flagged_points_dict` 中对应部件的集合里。
                        flagged_points_dict[part_name].update(points)
    # 调用内部函数 `process_kw_nodes` 处理 `kw1` 参数。
    # `kw1` 中的节点被视为标记节点，因此 `is_flagged` 设置为 True。
    process_kw_nodes(kw1, is_flagged=True)
    # 调用内部函数 `process_kw_nodes` 处理 `kw2` 参数。
    # `kw2` 中的节点被视为普通节点，因此 `is_flagged` 默认为 False。
    process_kw_nodes(kw2)
    # 格式转换：将 `flagged_points_dict` 中的集合转换为排序后的列表。
    # 这样可以确保输出的标记点是唯一且有序的。
    sorted_flagged = {
        part: sorted(list(points_set)) # 将集合转换为列表并排序
        for part, points_set in flagged_points_dict.items()
    }
    
    # 返回处理后的节点标签元组和排序后的标记点字典。
    return tuple(node_labels), sorted_flagged

# 定义一个函数，用于解析范围表达式字符串，例如 "1:5:2" (从1到5，步长为2)。
# `expr` 是一个字符串，表示一个或多个整数或一个范围。
def parse_range(expr):
    """解析范围表达式为具体数值列表"""
    # 使用冒号分割表达式字符串。
    parts = expr.split(':')
    try:
        # 如果分割后只有一个部分，表示这是一个单个的数值。
        if len(parts) == 1:  # 单个数值
            # 将其转换为整数并放入列表中返回。
            return [int(parts[0])]
        # 如果有多个部分，则解析为范围参数。
        start = int(parts[0]) # 范围的起始值。
        end = int(parts[1])   # 范围的结束值。
        # 步长，如果 `parts` 有三个部分，则取第三部分，否则默认为 1。
        step = int(parts[2]) if len(parts) == 3 else 1
        # 调整结束边界，以确保 `range` 函数包含 `end` 值。
        if step > 0:
            # 如果步长为正，`range` 函数的结束值需要加 1 才能包含 `end`。
            adjusted_end = end + 1
        elif step < 0:
            # 如果步长为负，`range` 函数的结束值需要减 1 才能包含 `end`。
            adjusted_end = end - 1
        else:  # 无效步长
            # 如果步长为 0，则认为表达式无效，返回空列表。
            return []
        # 使用 `range` 函数生成数值序列，并将其转换为列表返回。
        return list(range(start, adjusted_end, step))
    # 捕获 `ValueError` (例如，字符串无法转换为整数) 或 `IndexError` (例如，缺少范围参数) 异常。
    except (ValueError, IndexError):
        # 如果发生任何解析错误，则返回空列表，表示表达式无效。
        return []  # 跳过无效格式

def creatPath(pthname,expression):
    pth=session.Path(name=pthname, type=NODE_LIST, expression=expression)
    return pth

def build_variable_parameters(variable_inputs):
    '''
    传入字典列表，按名称及不变量/分量设置variables参数值
    user_variables = [
        {
            'name': 'CE',
            'output_position':'INTEGRATION_POINT',
            'invariants': ['Max. In-Plane Principal (Abs)'],
            'components': ['CE22']
        },
        {
            'name': 'S',
            'invariants': ['Mises', 'Tresca']
        },
        {
            'name': 'UVARM10'
        }]
    '''
    variables = []
    for var in variable_inputs:
        name = var['name']
        output_pos = var.get('output_position', 'INTEGRATION_POINT')#此处op_pos指变量名后的默认INTEGRATION_POINT
        """
        Possible values are ELEMENT_CENTROID, ELEMENT_FACE, ELEMENT_NODAL, GENERAL_PARTICLE, INTEGRATION_POINT, NODAL, WHOLE_ELEMENT, WHOLE_MODEL, WHOLE_PART_INSTANCE, and WHOLE_REGION.
        """
        output_pos_const = globals()[output_pos.upper()]
        
        entries = []
        for inv in var.get('invariants', []):
            entries.append((INVARIANT, inv))
        for comp in var.get('components', []):
            entries.append((COMPONENT, comp))
        
        if entries:
            var_tuple = (name, output_pos_const, tuple(entries))
        else:
            var_tuple = (name, output_pos_const)
        variables.append(var_tuple)
    return tuple(variables)

def generate_xy_data_args(odb, output_position, variables, node_labels):
    '''
    用于点xy输出参数打包
    返回参数字典供解包使用
    output_pos_const:ELEMENT_CENTROID/INTEGRATION_POINT/ELEMENT_NODAL/NODAL
    node_labels已经处理好了
    variables调用build_variable_parameters处理
    '''
    output_pos_const = globals()[output_position.upper()]
    variable_args = build_variable_parameters(variables)
    nodelabel_args = node_labels
    return {
        'odb': odb,
        'outputPosition': output_pos_const,
        'variable': variable_args,
        'nodeLabels': nodelabel_args
    }

def generate_xypath_data_args(name,pth,step,fram, variables,extraconfigs={}):
    '''
    用于路径xy输出参数打包
    name:保存XYData名
    pth:路径对象
    step,frame:整型
    user_variables = [
        {
            'name': 'CE',
            'invariants': ['Max. In-Plane Principal (Abs)'],
            'components': ['CE22']
        },
        {
            'name': 'S',
            'invariants': ['Mises', 'Tresca']
        },
        {
            'name': 'UVARM10'
        }]
    extraconfigs={
        'includeIntersections':False,
        'pathStyle':UNIFORM_SPACING, 
        'numIntervals':100, 
        'shape':UNDEFORMED, 
        'labelType':TRUE_DISTANCE, 
        'removeDuplicateXYPairs':True, 
        }
    '''
    args={}
    args.setdefault('includeIntersections',True)
    args.setdefault('shape',UNDEFORMED)
    args.setdefault('pathStyle',UNIFORM_SPACING)
    args.setdefault('numIntervals',100)
    args.setdefault('labelType',TRUE_DISTANCE)#默认值
    args.setdefault('removeDuplicateXYPairs',True)#默认值
    if args['pathStyle']==PATH_POINTS:
        args.setdefault('projectOntoMesh',True)
        args.setdefault('projectionTolerance',0)
    for key,value in extraconfigs.items():
        if isinstance(value, str):
            try:
                extraconfigs[key]=globals()[value.upper()]
            except:
                pass
    args.update(extraconfigs)
    variable_args = build_variable_parameters(variables)
    args.update({'name':name, #整合参数
            'path':pth,
            'step':step,
            'frame':fram,
            'variable':variable_args,
            })
    # print(args['name'])
    return args
    
def getxyData_point(args):
    '''
    args为generate_xy_data_args()返回的字典
    '''
    xyDataObjectsList=[]
    try:
        xyDataObjectsList=session.xyDataListFromField(**args)
    except VisError:
        pass
    if isinstance(xyDataObjectsList, list):
        # 如果返回值是列表，使用 extend() 方法添加到既有列表中
        # xyDataObjectsList.extend(xyDataObjectsList)
        pass
    else:
        # 如果返回值是单个对象，使用 append() 方法添加到既有列表中
        # xyDataObjectsList.append(xyDataObjectsList)
        xyDataObjectsList = [xyDataObjectsList]
    return xyDataObjectsList

# 定义一个函数，用于从 ODB 路径中获取 XY 数据对象列表。
# `args` 是由 `generate_xypath_data_args()` 函数返回的参数字典。
def getxyData_path(args):
    '''
    args为 generate_xypath_data_args()返回的字典
    '''
    # 初始化一个空列表，用于存储提取到的 XYData 对象。
    xyDataObjectsList=[]
    try:
        # 调用 `session.XYDataFromPath` 方法，使用解包的 `args` 字典作为参数。
        # 这个方法从 ODB 路径中提取数据并创建 XYData 对象。
        xyDataObjectsList=session.XYDataFromPath(**args)
    except OdpError:
        # 捕获 `OdpError` (ODB 访问错误)，通常表示指定的字段或路径不存在。
        # 打印错误信息，指出哪个变量字段未找到。
        print("filed {} not found".format(str(args['variable'])))
    # 检查 `xyDataObjectsList` 的类型。`session.XYDataFromPath`  返回列表或单个对象。
    if isinstance(xyDataObjectsList, list):
        # 如果返回值已经是列表，则不需要额外处理。
        # 如果返回值是列表，使用 extend() 方法添加到既有列表中
        # xyDataObjectsList.extend(xyDataObjectsList) # 这行是多余的，因为 xyDataObjectsList 已经是结果
        pass
    else:
        # 如果返回值是单个对象（而不是列表），则将其包装在一个列表中。
        # 如果返回值是单个对象，使用 append() 方法添加到既有列表中
        # xyDataObjectsList.append(xyDataObjectsList) # 这行是错误的，会导致无限递归
        xyDataObjectsList = [xyDataObjectsList] # 正确的做法是将其包装成一个列表
    # 返回包含 XYData 对象的列表。
    return xyDataObjectsList

# 接受六个应变分量作为输入：e_xx, e_yy, e_zz (正应变), gamma_xy, gamma_xz, gamma_yz (工程剪应变)。
def max_principal_strain(e_xx, e_yy, e_zz, gamma_xy, gamma_xz, gamma_yz):
    # 将工程剪应变转换为张量分量（剪应变张量分量是工程剪应变的一半）。
    e_xy = gamma_xy / 2.0
    e_xz = gamma_xz / 2.0
    e_yz = gamma_yz / 2.0
    
    # 构造应变张量矩阵（一个 3x3 的对称矩阵）。
    strain_tensor = np.array([
        [e_xx,  e_xy,  e_xz],
        [e_xy,  e_yy,  e_yz],
        [e_xz,  e_yz,  e_zz]
    ])
    
    # 计算应变张量的特征值。
    # `np.linalg.eigvalsh` 用于计算 Hermitian (或实对称) 矩阵的特征值，并按升序排列。
    eigenvalues = np.linalg.eigvalsh(strain_tensor)
    
    # 返回最大主应变，即排序后的特征值列表的最后一个元素。
    return eigenvalues[-1]

# 定义一个函数，用于执行基于 NumPy 的线性回归（或类似计算）。
# `points` 预期是一个包含 (x, y) 对的列表或 NumPy 数组。
def numpy_linear_regression(points):
    # 将输入点数据解包为 x 和 y 坐标的 NumPy 数组。
    # `.T` 用于转置数组，使得 `x` 包含所有 x 坐标，`y` 包含所有 y 坐标。
    x, y = np.array(points).T
    # 以下是被注释掉的传统线性回归代码，使用 `np.polyfit` 来计算斜率和截距。
    # slope, intercept = np.polyfit(x, y, 1)
    # return y.mean(), slope*x.min()+intercept, slope*x.max()+intercept
    # 计算 x 坐标范围的长度。
    t=x.max()-x.min()
    # 计算 x 坐标范围的中心点。
    c=(x.max()+x.min())/2
    # 计算平均应力（或类似量），使用梯形法则进行数值积分。
    # `np.trapz` 计算沿给定轴的积分。
    sigma_m=np.trapz(x=x,y=y)/t
    # 计算弯曲应力（或类似量），这里是基于矩的计算，常用于梁理论。
    sigma_b=-6.0*np.trapz(x=x,y=y*(x-c))/t**2
    # 返回平均应力、最大应力（平均应力 + 弯曲应力）和最小应力（平均应力 - 弯曲应力）。
    return sigma_m,sigma_m+sigma_b,sigma_m-sigma_b


def write_to_tsv(filename, data, is_first_write=False):
    # 打开文件，根据is_first_write决定模式
    if is_first_write:
        mode = 'w'  # 首次写入，清空文件
    else:
        mode = 'a'  # 后续写入，续写模式
    
    # 打开文件
    file = open(filename, mode, encoding='utf-8')
    
    # 写入数据
    for row in data:
        file.write('\t'.join(map(str, row)) + '\n')
    
    # 关闭文件
    file.close()

def ActiveStepsFrames(SSb='all',SFb='all',bstep=0,cstep=1,astep=0,f_str='0:-1',name=''):
    # 获取当前 ODB 数据对象，该对象包含关于 ODB 步和帧的信息。
    odbData=get_current_odbdata()
    # 帧选择逻辑。
    if SFb=='all':
        # 如果选择所有帧，则帧字符串为 '0:-1' (从第一个到最后一个)。
        fstr='0:-1'
    elif SFb=='range':
        # 如果选择指定范围的帧，则使用传入的 `f_str`。
        fstr = f_str
    elif SFb=='last':
        # 如果选择最后一帧，则帧字符串为 '-1:-1'。
        fstr='-1:-1'
    # 使用 `ast.literal_eval` 安全地将帧字符串转换为元组，例如 "('0:-1',)"。
    # 这样可以避免直接使用 `eval`  带来的安全风险。
    fcell=ast.literal_eval("('{}',)".format(fstr))
    # 步选择逻辑。
    SelectCell=[] # 初始化一个空列表，用于存储最终选定的步和帧元组。
    # 获取 ODB 中所有步的名称，并根据 `bstep` 和 `astep` 进行切片。
    # `odbData.steps.keys()` 返回一个包含所有步名称的列表。
    steps = odbData.steps.keys()[bstep:-astep if astep!=0 else None]
    if SSb == 'all':
        # 如果选择所有步，则为每个步创建一个 (步名, 帧元组) 条目。
        SelectCell = [(step, fcell) for step in steps]
    elif SSb == 'mod':
        # 如果选择模数步（按 `cstep` 间隔选择）。
        SelectCell = [
            (step, fcell) 
            for i, step in enumerate(steps, 1)  # `enumerate` 从 1 开始计数，用于模数判断。
            if i % cstep == 0 # 如果步的索引是 `cstep` 的倍数，则选择该步。
        ]
    elif SSb == 'Name' and name!='':
        # 如果按名称选择特定步。
        # print(name) # 调试语句，打印要查找的步名称。
        if name in odbData.steps.keys():
            # 如果指定的名称存在于步列表中，则只选择该步。
            SelectCell=[(name, fcell),]
        else:
            # 如果指定的名称不存在，则抛出 `ValueError`。
            # `.encode('GB18030')` 用于处理中文错误信息在某些环境下的显示问题。
            raise ValueError(u"没有名为{}的分析步，无法增补".format(name).encode('GB18030'))
        # SelectCell = [(step, fcell) for step in steps if name in step] # 原始注释掉的代码， 是用于模糊匹配。
        # print(SelectCell) # 调试语句，打印选定的步。
    elif SSb=='last':
        # 如果选择最后一个步。
        # `odbData.activeFrames[-1][0]`  是获取当前激活的最后一个步的名称。
        SelectCell = [(odbData.activeFrames[-1][0], fcell)]
    elif SSb == 'mod and last':
        # 如果选择模数步，并额外包含最后一个步。
        SelectCell = [
            (step, fcell) 
            for i, step in enumerate(steps, 1)  # `enumerate` 从 1 开始计数。
            if i % cstep == 0 # 如果步的索引是 `cstep` 的倍数，则选择该步。
        ]
        try:
            # 尝试添加 ODB 中最后一个步的名称。
            # SelectCell.append((odbData.activeFrames[-1][0], fcell)) # 原始注释掉的代码。
            SelectCell.append((odbData.steps.keys()[-1], fcell))
        except Exception as e:
            # 捕获异常并打印。
            print(e)
            # 如果获取最后一个步失败，则回退到使用 `odbData.activeFrames` 的最后一个步。
            SelectCell.append((odbData.activeFrames[-1][0], fcell))
    # 将构建好的 `SelectCell` 元组设置为 ODB 数据的激活帧。
    # 这会更新 Abaqus GUI 中显示的激活步和帧。
    odbData.setValues(activeFrames=tuple(SelectCell))


if __name__=='__main__':
    # tabledata={
    #     'Points':(#table1
    #         ('PART-1-1','1,5,7:13:2',True),
    #         ('PART-2-1','1',False),
    #         ),
    #     'Paths':(#table2
    #         ('Path-1','PART-1-1','12,4',False),
    #         ('Path-1','PART-2-1','15:21:2,30',True),
    #         ('Path-1','PART-1-1','33',True),
    #         ('Path-2','PART-1-1','1:5:2',True),
    #         )
    #     }
    tabledata={
        'Points':(#table1
            ('PART-2D-HALF-1','1082, 14354',True),
            ),
        'Paths':(#table2
            ('Path-1','PART-2D-HALF-1','1082, 14354',False),
            )
        }
    path_extras_configs={#用户希望路径输出
        'pathStyle':'UNIFORM_SPACING', 
        'numIntervals':100, 
        'shape':'UNDEFORMED', 
        }
    Field_configs={#变量位置
        'CreepDamage':'SDV27',
        'FatigueDamage':'SDV31',
        }
    Step_configs={
        'extrapolateTimes':370, #外推次数
        'extrapolateType':'Direct', #Add
        'addTypeStepNames':['Up-c3','Down-c4'],#增补分析步名称，暂不支持对路径IE增补
        'stepIDFs':[0,3,2], #循环前有几个分析步，每几步一个循环，最后几步不是循环
        'damageJudge':[0.1,0.1] #疲劳，蠕变判据
        }
    user_variables = [
    {
        'name': 'S',
        'invariants': ['Mises', 'Tresca']
    },]
    # o=get_current_odb()
    # p=o.rootAssembly.instances['PART-1-1']
    # n1=p.nodes
    # picks1=(n1[24],n1[25],)
    # picks2=(n1[1],)
    kernel_CreepFatigueDamage(tabledata,Field_configs,Step_configs,kw1=(),kw2=(),path_extras_configs=path_extras_configs)
    kernel_IE(tabledata,Field_configs,Step_configs,kw1=(),kw2=(),path_extras_configs=path_extras_configs)