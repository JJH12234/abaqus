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
datetimenow=str(datetime.datetime.now()).split('.')[0].replace(':',"'")
def kernal_analsys(analysetype,
           CreepDamageField,FatigueDamageField,CFICriterion,CreepDamageFieldnum,FatigueDamageFieldnum,
           pathStyle,numIntervals,shape,
           stepIDFs1,stepIDFs2,stepIDFs3,
           BrittleStress,
           extrapolateType,extrapolateTimes,addTypeStepNames,
           tabledata1=(),tabledata2=(),picks1=(),picks2=()):
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
        'addTypeStepNames':addTypeStepNames.split(','),#增补分析步名称，暂不支持对路径IE增补
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
        kernel_CreepFatigueDamage(tabledata,Field_configs,Step_configs,kw1=picks1,kw2=picks2)
    elif analysetype==u'防脆断分析'.encode('GB18030'):
        kernel_BrittleFailure(tabledata,Brittle_variables,path_extras_configs=path_extras_configs)
        # brittle_assess.run_gui()
        execfile('C:/Users/mrvoid/abaqus_plugins/Inelasticprogram/brittle_assess.py',__main__.__dict__)
def kernel_CreepFatigueDamage(tabledata,Field_configs,Step_configs,kw1=(),kw2=(),path_extras_configs={}):
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
        if field==CreDamUV:
            Damages[part][node]['CreepDamageByCycle']=[row[1] for row in xy.data]
        elif field==FatDamUV:
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
                delta=xy.data[-1][1]-xy.data[0][1]
                if field==CreDamUV:
                    Damages[part][node]['AddCreepDamage_'+stepname]=delta
                    Damages[part][node]['CreepDamageByCycle'][-1]+=delta
                elif field==FatDamUV:
                    Damages[part][node]['AddFatigueDamage_'+stepname]=delta
                    Damages[part][node]['FatigueDamageByCycle'][-1]+=delta
    for part in Damages:
        for node in Damages[part]:
            Damages[part][node]['judge']='Pass' if is_below_double_breakline(Damages[part][node]['FatigueDamageByCycle'][-1],Damages[part][node]['CreepDamageByCycle'][-1],Step_configs['damageJudge']) else 'NotPass'
    ###输出
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
                        if max(IE[part][node]['PE'+component])>IE[part][node]['PE'+component][-1]:
                            #对于最后一步PE分量并非最大值的情况，直接多次复制最大值
                            IE[part][node]['PE'+component]=IE[part][node]['PE'+component]+[max(IE[part][node]['PE'+component])]*Step_configs['extrapolateTimes']
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
                                    IE[part][node]['CE'+component][-1]+=delta
                                if field=='PE:PE{}'.format(component):
                                    pass#等待考虑
    for part in IE:
        for node in IE[part]:
            CEs=[ IE[part][node]['CE'+c][-1] if IE[part][node]['CE'+c] else 0 for c in components]
            PEs=[ IE[part][node]['PE'+c][-1] if IE[part][node]['PE'+c] else 0 for c in components]
            IEs=[ CEs[i] + PEs[i] for i in range(len(CEs)) ]
            IE[part][node]['FinalIEmax']=max_principal_strain(*IEs)
            if sortedflagged!={} and (node.split('node')[-1] in sortedflagged.get(part)):
                judge=0.025
            else:
                judge=0.05
            IE[part][node]['judge']='Pass' if IE[part][node]['FinalIEmax']<judge else 'NotPass >{}%'.format(str(judge*100))
    ###输出
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
            if ptype=='_ave':
                judge=0.005 if not flag else 0.01
            else:
                judge=0.01 if not flag else 0.02
            IE[pthname]['judge'+ptype]='Pass' if IE[pthname]['FinalIEmax'+ptype]<judge else 'NotPass >{}%'.format(str(judge*100))
    ###输出
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
    for item in processd_path:
        pthname,expression,flag=item
        ###循环创建分析步
        pth=creatPath(pthname,expression)
        file_Sname='{}_{} {}.txt'.format(pthname,user_variables[0]['name'],datetimenow)
        file_Tname='{}_{} {}.txt'.format(pthname,user_variables[1]['name'],datetimenow)
        file_S = open(file_Sname, 'w')
        file_T = open(file_Tname, 'w')
        # 遍历每个激活的分析步及其增量步
        total_time = 0.0
        for step_info in odbData.activeFrames:
            step_name, frame_exprs = step_info
            step_idx = step_indices[step_name]  # 外循环的索引
            for i in range(step_idx):
                total_time += odb.steps.values()[i].totalTime  # 累加每个Step的总时间
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
            for counter,frame_idx in enumerate(all_frame_indices,1):
                current_time = odb.steps.values()[step_idx].frames[frame_idx].frameValue
                total_time += current_time
                # 处理step_idx和frame_idx的组合
                XYname="{}__step{}__frame{}".format(pthname,step_idx,frame_idx)
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
                    elif varname=='NT11':
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

def process_points_data(pointdata,kw1=(),kw2=()):
    node_labels = []
    flagged_points_dict = defaultdict(set)  # 使用集合自动去重
    for item in pointdata:
        part_name, nodes_str, flag = item
        # 分割节点表达式并清理空格
        node_exprs = [expr.strip() for expr in nodes_str.split(',') if expr.strip()]
        # 构建节点参数结构
        for expr in node_exprs:
            node_labels.append((part_name, (expr,)))
        # 处理标记点（仅当flag为True时）
        if flag:
            for expr in node_exprs:
                points = parse_range(expr)
                if points:
                    flagged_points_dict[part_name].update(points)
    # 处理kw参数的新函数
    def process_kw_nodes(kw_nodes, is_flagged=False):
        for node in kw_nodes:
            part_name = node.instanceName
            if part_name in get_current_odb().rootAssembly.instances:
                expr = str(node.label).strip()
                # 添加节点参数
                node_labels.append((part_name, (expr,)))
                # 处理标记点
                if is_flagged:
                    points = parse_range(expr)
                    if points:
                        flagged_points_dict[part_name].update(points)
    # 处理kw1（标记节点）
    process_kw_nodes(kw1, is_flagged=True)
    # 处理kw2（普通节点）
    process_kw_nodes(kw2)
    # 格式转换
    sorted_flagged = {
        part: sorted(points_set)
        for part, points_set in flagged_points_dict.items()
    }
    
    return tuple(node_labels), sorted_flagged

def parse_range(expr):
    """解析范围表达式为具体数值列表"""
    parts = expr.split(':')
    try:
        if len(parts) == 1:  # 单个数值
            return [int(parts[0])]
        # 解析范围参数
        start = int(parts[0])
        end = int(parts[1])
        step = int(parts[2]) if len(parts) == 3 else 1
        # 调整边界确保包含终点
        if step > 0:
            adjusted_end = end + 1
        elif step < 0:
            adjusted_end = end - 1
        else:  # 无效步长
            return []
        return list(range(start, adjusted_end, step))
    except (ValueError, IndexError):
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
        xyDataObjectsList.extend(xyDataObjectsList)
    else:
        # 如果返回值是单个对象，使用 append() 方法添加到既有列表中
        xyDataObjectsList.append(xyDataObjectsList)
    return xyDataObjectsList

def getxyData_path(args):
    '''
    args为 generate_xypath_data_args()返回的字典
    '''
    xyDataObjectsList=[]
    try:
        xyDataObjectsList=session.XYDataFromPath(**args)
    except OdpError:
        print("filed {} not found".format(str(args['variable'])))
    if isinstance(xyDataObjectsList, list):
        # 如果返回值是列表，使用 extend() 方法添加到既有列表中
        xyDataObjectsList.extend(xyDataObjectsList)
    else:
        # 如果返回值是单个对象，使用 append() 方法添加到既有列表中
        xyDataObjectsList.append(xyDataObjectsList)
    return xyDataObjectsList

def max_principal_strain(e_xx, e_yy, e_zz, gamma_xy, gamma_xz, gamma_yz):
    # 将工程剪应变转换为张量分量
    e_xy = gamma_xy / 2.0
    e_xz = gamma_xz / 2.0
    e_yz = gamma_yz / 2.0
    
    # 构造应变张量矩阵
    strain_tensor = np.array([
        [e_xx,  e_xy,  e_xz],
        [e_xy,  e_yy,  e_yz],
        [e_xz,  e_yz,  e_zz]
    ])
    
    # 计算特征值（按升序排列）
    eigenvalues = np.linalg.eigvalsh(strain_tensor)
    
    # 返回最大主应变（最后一个元素）
    return eigenvalues[-1]

def numpy_linear_regression(points):
    x, y = np.array(points).T
    slope, intercept = np.polyfit(x, y, 1)
    return y.mean(), slope*x.min()+intercept, slope*x.max()+intercept

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
    odbData=get_current_odbdata()
    #帧选择
    if SFb=='all':
        fstr='0:-1'
    elif SFb=='range':
        fstr = f_str
    elif SFb=='last':
        fstr='-1:-1'
    fcell=ast.literal_eval("('{}',)".format(fstr))
    #步选择
    SelectCell=[]
    steps = odbData.steps.keys()[bstep:-astep if astep!=0 else -1]
    if SSb == 'all':
        SelectCell = [(step, fcell) for step in steps]
    elif SSb == 'mod':
        SelectCell = [
            (step, fcell) 
            for i, step in enumerate(steps, 1)  # python2.7 start参数写法
            if i % cstep == 0
        ]
    elif SSb == 'Name' and name!='':
        # print(name)
        if name in odbData.steps.keys():
            SelectCell=[(name, fcell),]
        else:
            raise ValueError(u"没有名为{}的分析步，无法增补".format(name).encode('GB18030'))
        # SelectCell = [(step, fcell) for step in steps if name in step]
        # print(SelectCell)
    elif SSb=='last':
        SelectCell = [(odbData.activeFrames[-1][0], fcell)]
    elif SSb == 'mod and last':
        SelectCell = [
            (step, fcell) 
            for i, step in enumerate(steps, 1)  # python2.7 start参数写法
            if i % cstep == 0
        ]
        try:
            # SelectCell.append((odbData.activeFrames[-1][0], fcell))
            SelectCell.append((odbData.steps.keys()[-1], fcell))
        except:
            SelectCell.append((odbData.activeFrames[-1][0], fcell))
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