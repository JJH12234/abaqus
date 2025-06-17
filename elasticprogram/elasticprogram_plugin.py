# -*- coding: utf-8 -*-
from abaqusGui import *
from abaqusConstants import ALL
import osutils, os
import os, sys
thisDir = os.path.dirname(os.path.abspath(__file__))
###########################################################################
# Class definition
###########################################################################

class Softwareprogram_plugin(AFXForm):

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def __init__(self, owner):
        
        # Construct the base class.
        #
        AFXForm.__init__(self, owner)
        self.radioButtonGroups = {}
        self.cmd = AFXGuiCommand(self, 'kernal_analsys', 'postprocess')
        # self.cmd2 = AFXGuiCommand(self, __name__, 'Softwareprogram_plugin._run_brittle_assess')
        # self.cmd2 = AFXGuiCommand(self, '_run_brittle_assess', __name__)
        # self.cmd2 = AFXGuiCommand(self, __name__, '_run_brittle_assess')
        pickedDefault = ''
        self.analysetypeKw = AFXStringKeyword(self.cmd, 'analysetype', True, u'非弹性应变'.encode('GB18030'))
        self.CreepDamageFieldKw = AFXStringKeyword(self.cmd, 'CreepDamageField', True, 'UVARM')
        self.FatigueDamageFieldKw = AFXStringKeyword(self.cmd, 'FatigueDamageField', True, 'UVARM')
        self.CFICriterionKw = AFXStringKeyword(self.cmd, 'CFICriterion', True, '0.3,0.3')
        self.CreepDamageFieldnumKw = AFXIntKeyword(self.cmd, 'CreepDamageFieldnum', True, 27)
        self.FatigueDamageFieldnumKw = AFXIntKeyword(self.cmd, 'FatigueDamageFieldnum', True, 31)
        self.pathStyleKw = AFXStringKeyword(self.cmd, 'pathStyle', True, 'UNIFORM_SPACING')
        self.numIntervalsKw = AFXIntKeyword(self.cmd, 'numIntervals', True, 100)
        if not self.radioButtonGroups.has_key('shape'):
            self.shapeKw1 = AFXIntKeyword(None, 'shapeDummy', True)
            self.shapeKw2 = AFXStringKeyword(self.cmd, 'shape', True)
            self.radioButtonGroups['shape'] = (self.shapeKw1, self.shapeKw2, {})
        self.radioButtonGroups['shape'][2][29] = 'UNDEFORMED'
        self.shapeKw1.setValue(29)
        if not self.radioButtonGroups.has_key('shape'):
            self.shapeKw1 = AFXIntKeyword(None, 'shapeDummy', True)
            self.shapeKw2 = AFXStringKeyword(self.cmd, 'shape', True)
            self.radioButtonGroups['shape'] = (self.shapeKw1, self.shapeKw2, {})
        self.radioButtonGroups['shape'][2][30] = 'DEFORMED'
        self.stepIDFs1Kw = AFXIntKeyword(self.cmd, 'stepIDFs1', True, 0)
        self.stepIDFs2Kw = AFXIntKeyword(self.cmd, 'stepIDFs2', True, 7)
        self.stepIDFs3Kw = AFXIntKeyword(self.cmd, 'stepIDFs3', True, 2)
        self.BrittleStressKw = AFXStringKeyword(self.cmd, 'BrittleStress', True, 'Mises')
        self.extrapolateTypeKw = AFXStringKeyword(self.cmd, 'extrapolateType', True, 'Add')
        self.extrapolateTimesKw = AFXIntKeyword(self.cmd, 'extrapolateTimes', True, 365)
        self.addTypeStepNamesKw = AFXStringKeyword(self.cmd, 'addTypeStepNames', True, 'G6-Cycle10,G13-Cycle10,G9-Cycle10')
        self.picks1Kw = AFXObjectKeyword(self.cmd, 'picks1', TRUE, pickedDefault)
        self.picks2Kw = AFXObjectKeyword(self.cmd, 'picks2', TRUE, pickedDefault)
        # plugin.py 恢复为 AFXTableKeyword
        # self.tabledata1Kw = AFXObjectKeyword(self.cmd, 'tabledata1', True)
        # self.tabledata2Kw = AFXObjectKeyword(self.cmd, 'tabledata2', True)
        self.tabledata1Kw = AFXTableKeyword(self.cmd, 'tabledata1', False)
        self.tabledata2Kw = AFXTableKeyword(self.cmd, 'tabledata2', False)

        # 设置列类型（与表格列对应）
        self.tabledata1Kw.setColumnType(0, AFXTABLE_TYPE_STRING)  # 实例名
        self.tabledata1Kw.setColumnType(1, AFXTABLE_TYPE_STRING)  # 节点标签
        self.tabledata1Kw.setColumnType(2, AFXTABLE_TYPE_BOOL)    # 焊缝标记

        self.tabledata2Kw.setColumnType(0, AFXTABLE_TYPE_STRING)  # 路径名
        self.tabledata2Kw.setColumnType(1, AFXTABLE_TYPE_STRING)  # 实例名
        self.tabledata2Kw.setColumnType(2, AFXTABLE_TYPE_STRING)  # 节点标签
        self.tabledata2Kw.setColumnType(3, AFXTABLE_TYPE_BOOL)    # 焊缝标记
    def _run_brittle_assess(self):
        script = r"D:/SIMULIA/EstProducts/2023/win_b64/code/python2.7/" \
                 r"lib/abaqus_plugins/elasticprogram/brittle_assess.py"
        cmd = (
            "import sys, runpy, traceback\n"
            "sys.modules.pop('brittle_assess', None)\n"
            "print '>>> running brittle_assess.py'\n"
            "try:\n"
            "    runpy.run_path(r'%s', run_name='__main__')\n"
            "except Exception:\n"
            "    traceback.print_exc()\n"
        ) % script.replace('\\', '\\\\')
        sendCommand(cmd)
        getAFXApp().getAFXMainWindow().writeToMessageArea(
            "11111【Tinker】脚本命令已发送，请在 Kernel Command 视图查看运行日志\n"
        )
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def getFirstDialog(self):

        import elasticprogramDB
        reload(elasticprogramDB)
        return elasticprogramDB.SoftwareprogramDB(self)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def doCustomChecks(self):

        # Try to set the appropriate radio button on. If the user did
        # not specify any buttons to be on, do nothing.
        #
        for kw1,kw2,d in self.radioButtonGroups.values():
            try:
                value = d[ kw1.getValue() ]
                kw2.setValue(value)
            except:
                pass
        return True

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def okToCancel(self):

        # No need to close the dialog when a file operation (such
        # as New or Open) or model change is executed.
        #
        return False
    def applyPressed(self):
        # 发送第一个命令
        sendCommand(self.cmd.getCommandString())
        
        # 当分析类型为脆性破坏时执行第二个任务
        if self.analysetypeKw.getValue() == 'Brittle failure':
            # 直接调用脆性评估方法
            self._run_brittle_assess()
        
        return AFXForm.applyPressed(self)

    def okPressed(self):
        # 发送第一个命令
        sendCommand(self.cmd.getCommandString())
        
        # 当分析类型为脆性破坏时执行第二个任务
        if self.analysetypeKw.getValue() == 'Brittle failure':
            # 直接调用脆性评估方法
            self._run_brittle_assess()
        
        return AFXForm.okPressed(self)
    

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Register the plug-in
#
thisPath = os.path.abspath(__file__)
thisDir = os.path.dirname(thisPath)

toolset = getAFXApp().getAFXMainWindow().getPluginToolset()
toolset.registerGuiMenuButton(
    buttonText='InelasticAnalsis|analysis', 
    object=Softwareprogram_plugin(toolset),
    messageId=AFXMode.ID_ACTIVATE,
    icon=None,
    kernelInitString  = 'import postprocess',
    applicableModules=ALL,
    version='N/A',
    author='N/A',
    description='N/A',
    helpUrl='N/A'
)
