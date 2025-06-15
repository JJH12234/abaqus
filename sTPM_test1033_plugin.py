from abaqusGui import *
from abaqusConstants import ALL
import osutils, os


###########################################################################
# Class definition
###########################################################################

class STPM_test1033_plugin(AFXForm):

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def __init__(self, owner):
        
        # Construct the base class.
        #
        AFXForm.__init__(self, owner)
        self.radioButtonGroups = {}
        
        self.cmd = AFXGuiCommand(mode=self, method='fun1',
            objectName='ECUST', registerQuery=False)
        pickedDefault = ''
        self.fileNameKw = AFXStringKeyword(self.cmd, 'fileName', True, 'ParaModelingData.xls')
        self.keyword95Kw = AFXStringKeyword(self.cmd, 'keyword95', True, 'XGB')
        self.keyword97Kw = AFXTableKeyword(self.cmd, 'keyword97', True)
        self.keyword97Kw.setColumnType(0, AFXTABLE_TYPE_STRING)
        self.keyword97Kw.setColumnType(1, AFXTABLE_TYPE_STRING)
        self.keyword97Kw.setColumnType(2, AFXTABLE_TYPE_STRING)
        self.keyword97Kw.setColumnType(3, AFXTABLE_TYPE_STRING)
        self.keyword97Kw.setColumnType(4, AFXTABLE_TYPE_STRING)
        self.JSONNameKw = AFXStringKeyword(self.cmd, 'JSONName', True, 'MaterialData.json')
        self.keyword88Kw = AFXStringKeyword(self.cmd, 'keyword88', True, '2.25Cr1Mo')
        self.keyword47Kw = AFXStringKeyword(self.cmd, 'keyword47', True, '')
        self.keyword98Kw = AFXStringKeyword(self.cmd, 'keyword98', True, 'M225Cr1Mo')
        self.keyword99Kw = AFXStringKeyword(self.cmd, 'keyword99', True, '')
        self.keyword100Kw = AFXStringKeyword(self.cmd, 'keyword100', True, '')
        self.keyword61Kw = AFXIntKeyword(self.cmd, 'keyword61', True, 4)
        self.keyword62Kw = AFXIntKeyword(self.cmd, 'keyword62', True, 4)
        if not self.radioButtonGroups.has_key('DataXYType'):
            self.DataXYTypeKw1 = AFXIntKeyword(None, 'DataXYTypeDummy', True)
            self.DataXYTypeKw2 = AFXStringKeyword(self.cmd, 'DataXYType', True)
            self.radioButtonGroups['DataXYType'] = (self.DataXYTypeKw1, self.DataXYTypeKw2, {})
        self.radioButtonGroups['DataXYType'][2][53] = 'Separated'
        if not self.radioButtonGroups.has_key('DataXYType'):
            self.DataXYTypeKw1 = AFXIntKeyword(None, 'DataXYTypeDummy', True)
            self.DataXYTypeKw2 = AFXStringKeyword(self.cmd, 'DataXYType', True)
            self.radioButtonGroups['DataXYType'] = (self.DataXYTypeKw1, self.DataXYTypeKw2, {})
        self.radioButtonGroups['DataXYType'][2][54] = 'Unified X'
        self.DataXYTypeKw1.setValue(54)
        if not self.radioButtonGroups.has_key('DataTimeType'):
            self.DataTimeTypeKw1 = AFXIntKeyword(None, 'DataTimeTypeDummy', True)
            self.DataTimeTypeKw2 = AFXStringKeyword(self.cmd, 'DataTimeType', True)
            self.radioButtonGroups['DataTimeType'] = (self.DataTimeTypeKw1, self.DataTimeTypeKw2, {})
        self.radioButtonGroups['DataTimeType'][2][55] = 'Step Time'
        self.DataTimeTypeKw1.setValue(55)
        if not self.radioButtonGroups.has_key('TimeUnit'):
            self.TimeUnitKw1 = AFXIntKeyword(None, 'TimeUnitDummy', True)
            self.TimeUnitKw2 = AFXStringKeyword(self.cmd, 'TimeUnit', True)
            self.radioButtonGroups['TimeUnit'] = (self.TimeUnitKw1, self.TimeUnitKw2, {})
        self.radioButtonGroups['TimeUnit'][2][56] = 'Time(h)'
        self.TimeUnitKw1.setValue(56)
        if not self.radioButtonGroups.has_key('TimeUnit'):
            self.TimeUnitKw1 = AFXIntKeyword(None, 'TimeUnitDummy', True)
            self.TimeUnitKw2 = AFXStringKeyword(self.cmd, 'TimeUnit', True)
            self.radioButtonGroups['TimeUnit'] = (self.TimeUnitKw1, self.TimeUnitKw2, {})
        self.radioButtonGroups['TimeUnit'][2][57] = 'Time(s)'
        self.InputDataNameKw = AFXStringKeyword(self.cmd, 'InputDataName', True, 'InputData.xls')
        self.keyword64Kw = AFXStringKeyword(self.cmd, 'keyword64', True, '')
        self.keyword68Kw = AFXStringKeyword(self.cmd, 'keyword68', True, '')
        self.keyword65Kw = AFXStringKeyword(self.cmd, 'keyword65', True, 'G5,Steady,G6,Steady,G13,Steady,G9')
        self.keyword69Kw = AFXStringKeyword(self.cmd, 'keyword69', True, 'HOLDING')
        self.keyword77Kw = AFXIntKeyword(self.cmd, 'keyword77', True, 15)
        self.keyword94Kw = AFXFloatKeyword(self.cmd, 'keyword94', True, 1000)
        if not self.radioButtonGroups.has_key('HFrame33'):
            self.HFrame33Kw1 = AFXIntKeyword(None, 'HFrame33Dummy', True)
            self.HFrame33Kw2 = AFXStringKeyword(self.cmd, 'HFrame33', True)
            self.radioButtonGroups['HFrame33'] = (self.HFrame33Kw1, self.HFrame33Kw2, {})
        self.radioButtonGroups['HFrame33'][2][58] = 'NEW'
        if not self.radioButtonGroups.has_key('HFrame33'):
            self.HFrame33Kw1 = AFXIntKeyword(None, 'HFrame33Dummy', True)
            self.HFrame33Kw2 = AFXStringKeyword(self.cmd, 'HFrame33', True)
            self.radioButtonGroups['HFrame33'] = (self.HFrame33Kw1, self.HFrame33Kw2, {})
        self.radioButtonGroups['HFrame33'][2][59] = 'REPLACE'
        self.HFrame33Kw1.setValue(59)
        self.keyword90Kw = AFXStringKeyword(self.cmd, 'keyword90', True, 'AUTO')
        if not self.radioButtonGroups.has_key('GroupBox23'):
            self.GroupBox23Kw1 = AFXIntKeyword(None, 'GroupBox23Dummy', True)
            self.GroupBox23Kw2 = AFXStringKeyword(self.cmd, 'GroupBox23', True)
            self.radioButtonGroups['GroupBox23'] = (self.GroupBox23Kw1, self.GroupBox23Kw2, {})
        self.radioButtonGroups['GroupBox23'][2][60] = '\xa1\xa1'
        self.GroupBox23Kw1.setValue(60)
        self.keyword72Kw = AFXStringKeyword(self.cmd, 'keyword72', True, 'Steady')
        if not self.radioButtonGroups.has_key('GroupBox23'):
            self.GroupBox23Kw1 = AFXIntKeyword(None, 'GroupBox23Dummy', True)
            self.GroupBox23Kw2 = AFXStringKeyword(self.cmd, 'GroupBox23', True)
            self.radioButtonGroups['GroupBox23'] = (self.GroupBox23Kw1, self.GroupBox23Kw2, {})
        self.radioButtonGroups['GroupBox23'][2][61] = 'or'
        self.keyword73Kw = AFXStringKeyword(self.cmd, 'keyword73', True, '')
        self.keyword70Kw = AFXStringKeyword(self.cmd, 'keyword70', True)
        self.keyword74Kw = AFXStringKeyword(self.cmd, 'keyword74', True, '')
        self.keyword92Kw = AFXStringKeyword(self.cmd, 'keyword92', True, '')
        self.keyword80Kw = AFXStringKeyword(self.cmd, 'keyword80', True, 'G5,Steady,G6,Steady,G13,Steady,G9')
        self.keyword93Kw = AFXStringKeyword(self.cmd, 'keyword93', True, '')
        self.keyword81Kw = AFXIntKeyword(self.cmd, 'keyword77', True, 15)
        self.keyword78Kw = AFXTableKeyword(self.cmd, 'keyword78', True)
        self.keyword78Kw.setColumnType(0, AFXTABLE_TYPE_STRING)
        self.keyword78Kw.setColumnType(1, AFXTABLE_TYPE_STRING)
        self.keyword78Kw.setColumnType(2, AFXTABLE_TYPE_STRING)
        self.keyword82Kw = AFXTableKeyword(self.cmd, 'keyword82', True)
        self.keyword82Kw.setColumnType(0, AFXTABLE_TYPE_FLOAT)
        self.keyword82Kw.setColumnType(1, AFXTABLE_TYPE_FLOAT)
        self.keyword83Kw = AFXTableKeyword(self.cmd, 'keyword83', True)
        self.keyword83Kw.setColumnType(0, AFXTABLE_TYPE_FLOAT)
        self.keyword83Kw.setColumnType(1, AFXTABLE_TYPE_FLOAT)
        self.keyword83Kw.setColumnType(2, AFXTABLE_TYPE_FLOAT)
        self.keyword83Kw.setColumnType(3, AFXTABLE_TYPE_FLOAT)
        self.keyword83Kw.setColumnType(4, AFXTABLE_TYPE_FLOAT)
        self.SubroutineNameKw = AFXStringKeyword(self.cmd, 'SubroutineName', True, '')
        self.keyword43Kw = AFXStringKeyword(self.cmd, 'keyword43', True, 'RCC')
        self.keyword16Kw = AFXTableKeyword(self.cmd, 'keyword16', True)
        self.keyword16Kw.setColumnType(0, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(1, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(2, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(3, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(4, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(5, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(6, AFXTABLE_TYPE_FLOAT)
        self.keyword35Kw = AFXTableKeyword(self.cmd, 'keyword35', True)
        self.keyword35Kw.setColumnType(0, AFXTABLE_TYPE_FLOAT)
        self.keyword35Kw.setColumnType(1, AFXTABLE_TYPE_FLOAT)
        self.keyword35Kw.setColumnType(2, AFXTABLE_TYPE_FLOAT)
        self.keyword35Kw.setColumnType(3, AFXTABLE_TYPE_FLOAT)
        self.keyword41Kw = AFXTableKeyword(self.cmd, 'keyword41', True)
        self.keyword41Kw.setColumnType(0, AFXTABLE_TYPE_FLOAT)
        self.keyword41Kw.setColumnType(1, AFXTABLE_TYPE_FLOAT)
        self.keyword41Kw.setColumnType(2, AFXTABLE_TYPE_FLOAT)
        self.keyword41Kw.setColumnType(3, AFXTABLE_TYPE_FLOAT)
        self.keyword08Kw = AFXIntKeyword(self.cmd, 'keyword08', True, 3)
        self.keyword07Kw = AFXIntKeyword(self.cmd, 'keyword07', True, 3)
        self.keyword30Kw = AFXFloatKeyword(self.cmd, 'keyword30', True)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def getFirstDialog(self):

        import sTPM_test1033DB
        # reload(sTPM_test1033DB)
        return sTPM_test1033DB.STPM_test1033DB(self)

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Register the plug-in
#
thisPath = os.path.abspath(__file__)
thisDir = os.path.dirname(thisPath)

toolset = getAFXApp().getAFXMainWindow().getPluginToolset()
toolset.registerGuiMenuButton(
    buttonText='InelasticAnalsis|model', 
    object=STPM_test1033_plugin(toolset),
    messageId=AFXMode.ID_ACTIVATE,
    icon=None,
    kernelInitString='import ECUST',
    applicableModules=ALL,
    version='N/A',
    author='N/A',
    description='N/A',
    helpUrl='N/A'
)
