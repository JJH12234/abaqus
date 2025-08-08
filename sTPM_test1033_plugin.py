# -*- coding: utf-8 -*-
# 导入 Abaqus GUI 相关的模块，用于创建图形用户界面 (Graphical User Interface, GUI) 组件。
from abaqusGui import *
# 导入 Abaqus 常量，例如 ALL，它通常用于指定适用于所有模块或所有类型的对象。
from abaqusConstants import ALL
# 导入 osutils 和 os 模块，这些模块提供了与操作系统交互的功能，例如文件路径操作。
import osutils, os


###########################################################################
# Class definition
###########################################################################

# 定义一个名为 STPM_test1033_plugin 的类。
# 它继承自 AFXForm，表明这是一个 Abaqus GUI 表单或对话框的实现。
class STPM_test1033_plugin(AFXForm):

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 类的构造函数。当插件被实例化时，此方法会自动调用。
    # owner 参数通常是拥有此表单的父对象，例如 Abaqus 的工具集。
    def __init__(self, owner):
        
        # 调用父类 AFXForm 的构造函数，完成基础的初始化设置。
        AFXForm.__init__(self, owner)
        # 初始化一个字典，用于管理和存储单选按钮组的状态。
        # 键是单选按钮组的名称，值通常是一个元组，包含用于内部ID和实际字符串值的关键字，以及一个ID到字符串的映射字典。
        self.radioButtonGroups = {}
        
        # 定义一个 AFXGuiCommand 对象。
        # 这是插件的核心，它定义了当用户提交表单时将执行的 Abaqus 命令。
        # mode=self: 指定命令的上下文是当前表单实例。
        # method='fun1': 指定在 Abaqus Kernel 中要执行的方法名。
        # objectName='ECUST': 指定包含 'fun1' 方法的模块或对象名称。这意味着 Abaqus Kernel 会尝试从 ECUST 模块中调用 fun1。
        # registerQuery=False: 表示此命令不注册为查询命令。
        self.cmd = AFXGuiCommand(mode=self, method='fun1',
            objectName='ECUST', registerQuery=False)
        # 初始化一个变量，可能用于存储默认选中的值，但在此处未被直接使用。
        pickedDefault = ''
        # 定义一个 AFXStringKeyword 关键字，用于在 GUI 中表示一个字符串输入字段。
        # 'fileName': 关键字的名称，它将作为参数名传递给 'fun1' 方法。
        # True: 表示该字段是必需的。
        # 'ParaModelingData.xls': 字段的默认值。
        self.fileNameKw = AFXStringKeyword(self.cmd, 'fileName', True, 'ParaModelingData.xls')
        # 定义另一个 AFXStringKeyword，可能用于某种模型或配置的标识符。
        # 'XGB': 默认值。
        self.keyword95Kw = AFXStringKeyword(self.cmd, 'keyword95', True, 'XGB')
        # 定义一个 AFXTableKeyword 关键字，用于在 GUI 中表示一个表格输入控件。
        # 表格允许用户输入多行多列的数据。
        self.keyword97Kw = AFXTableKeyword(self.cmd, 'keyword97', True)
        # 设置 keyword97Kw 表格的第一列的数据类型为字符串。
        self.keyword97Kw.setColumnType(0, AFXTABLE_TYPE_STRING)
        # 设置 keyword97Kw 表格的第二列的数据类型为字符串。
        self.keyword97Kw.setColumnType(1, AFXTABLE_TYPE_STRING)
        # 设置 keyword97Kw 表格的第三列的数据类型为字符串。
        self.keyword97Kw.setColumnType(2, AFXTABLE_TYPE_STRING)
        # 设置 keyword97Kw 表格的第四列的数据类型为字符串。
        self.keyword97Kw.setColumnType(3, AFXTABLE_TYPE_STRING)
        # 设置 keyword97Kw 表格的第五列的数据类型为字符串。
        self.keyword97Kw.setColumnType(4, AFXTABLE_TYPE_STRING)
        # 定义一个 AFXStringKeyword，用于存储 JSON 格式数据的文件名。
        # 'MaterialData.json': 默认值。
        self.JSONNameKw = AFXStringKeyword(self.cmd, 'JSONName', True, 'MaterialData.json')
        # 定义一个 AFXStringKeyword，可能用于存储材料的名称或类型。
        # '2.25Cr1Mo': 默认值。
        self.keyword88Kw = AFXStringKeyword(self.cmd, 'keyword88', True, '2.25Cr1Mo')
        # 定义一个 AFXStringKeyword，默认值为空字符串。
        self.keyword47Kw = AFXStringKeyword(self.cmd, 'keyword47', True, '')
        # 定义一个 AFXStringKeyword，可能用于存储模型的特定标识符。
        # 'M225Cr1Mo': 默认值。
        self.keyword98Kw = AFXStringKeyword(self.cmd, 'keyword98', True, 'M225Cr1Mo')
        # 定义一个 AFXStringKeyword，默认值为空字符串。
        self.keyword99Kw = AFXStringKeyword(self.cmd, 'keyword99', True, '')
        # 定义一个 AFXStringKeyword，默认值为空字符串。
        self.keyword100Kw = AFXStringKeyword(self.cmd, 'keyword100', True, '')
        # 定义一个 AFXIntKeyword，用于存储整数值。
        # 4: 默认值。
        self.keyword61Kw = AFXIntKeyword(self.cmd, 'keyword61', True, 4)
        # 定义另一个 AFXIntKeyword，用于存储整数值。
        # 4: 默认值。
        self.keyword62Kw = AFXIntKeyword(self.cmd, 'keyword62', True, 4)
        # 定义一个 AFXStringKeyword，可能用于存储温度场相关的设置或名称。
        # 默认值为空字符串。
        self.temperatureFieldKw = AFXStringKeyword(self.cmd, 'temperatureField', True, '')
        
        # 以下代码块用于设置名为 'DataXYType' 的单选按钮组。
        # 检查 self.radioButtonGroups 字典中是否已经存在 'DataXYType' 这个键。
        if not self.radioButtonGroups.has_key('DataXYType'):
            # 如果不存在，则创建两个关键字来管理这个单选按钮组：
            # DataXYTypeKw1: 一个 AFXIntKeyword，用于存储用户选择的单选按钮的内部整数 ID。
            # None: 表示这个关键字不直接与 AFXGuiCommand 关联，它是一个内部状态管理关键字。
            self.DataXYTypeKw1 = AFXIntKeyword(None, 'DataXYTypeDummy', True)
            # DataXYTypeKw2: 一个 AFXStringKeyword，用于存储与所选 ID 对应的实际字符串值，这个值将传递给 self.cmd。
            self.DataXYTypeKw2 = AFXStringKeyword(self.cmd, 'DataXYType', True)
            # 将这两个关键字和一个空字典（用于存储 ID 到字符串值的映射）添加到 self.radioButtonGroups 字典中。
            self.radioButtonGroups['DataXYType'] = (self.DataXYTypeKw1, self.DataXYTypeKw2, {})
        # 将内部 ID 53 映射到字符串 'Separated'。当用户选择 ID 为 53 的单选按钮时，DataXYTypeKw2 的值将变为 'Separated'。
        self.radioButtonGroups['DataXYType'][2][53] = 'Separated'
        # 再次检查 'DataXYType' 组是否已存在。这种重复的检查模式在自动生成的 GUI 代码中很常见。
        # 这种重复检查通常是 GUI 构建器生成的代码的特点，确保即使在不同代码路径下也能正确初始化。
        if not self.radioButtonGroups.has_key('DataXYType'):
            # 如果不存在，则再次创建关键字。
            self.DataXYTypeKw1 = AFXIntKeyword(None, 'DataXYTypeDummy', True)
            self.DataXYTypeKw2 = AFXStringKeyword(self.cmd, 'DataXYType', True)
            self.radioButtonGroups['DataXYType'] = (self.DataXYTypeKw1, self.DataXYTypeKw2, {})
        # 将内部 ID 54 映射到字符串 'Unified X'。
        self.radioButtonGroups['DataXYType'][2][54] = 'Unified X'
        # 设置 DataXYTypeKw1 的默认值为 54，这意味着在 GUI 初始化时，'Unified X' 选项将被默认选中。
        self.DataXYTypeKw1.setValue(54)
        
        # 以下代码块用于设置名为 'DataTimeType' 的单选按钮组。
        # 检查 'DataTimeType' 组是否已存在。
        if not self.radioButtonGroups.has_key('DataTimeType'):
            # 如果不存在，则创建相关关键字。
            self.DataTimeTypeKw1 = AFXIntKeyword(None, 'DataTimeTypeDummy', True)
            self.DataTimeTypeKw2 = AFXStringKeyword(self.cmd, 'DataTimeType', True)
            self.radioButtonGroups['DataTimeType'] = (self.DataTimeTypeKw1, self.DataTimeTypeKw2, {})
        # 将内部 ID 55 映射到字符串 'Step Time'。
        self.radioButtonGroups['DataTimeType'][2][55] = 'Step Time'
        # 设置 DataTimeTypeKw1 的默认值为 55，即默认选择 'Step Time'。
        self.DataTimeTypeKw1.setValue(55)
        
        # 以下代码块用于设置名为 'TimeUnit' 的单选按钮组。
        # 检查 'TimeUnit' 组是否已存在。
        if not self.radioButtonGroups.has_key('TimeUnit'):
            # 如果不存在，则创建相关关键字。
            self.TimeUnitKw1 = AFXIntKeyword(None, 'TimeUnitDummy', True)
            self.TimeUnitKw2 = AFXStringKeyword(self.cmd, 'TimeUnit', True)
            self.radioButtonGroups['TimeUnit'] = (self.TimeUnitKw1, self.TimeUnitKw2, {})
        # 将内部 ID 56 映射到字符串 'Time(h)' (小时)。
        self.radioButtonGroups['TimeUnit'][2][56] = 'Time(h)'
        # 设置 TimeUnitKw1 的默认值为 56，即默认选择 'Time(h)'。
        self.TimeUnitKw1.setValue(56)
        # 再次检查 'TimeUnit' 组是否已存在。
        # 这种重复检查模式在自动生成的 GUI 代码中很常见，确保即使在不同代码路径下也能正确初始化。
        if not self.radioButtonGroups.has_key('TimeUnit'):
            # 如果不存在，则再次创建关键字。
            self.TimeUnitKw1 = AFXIntKeyword(None, 'TimeUnitDummy', True)
            self.TimeUnitKw2 = AFXStringKeyword(self.cmd, 'TimeUnit', True)
            self.radioButtonGroups['TimeUnit'] = (self.TimeUnitKw1, self.TimeUnitKw2, {})
        # 将内部 ID 57 映射到字符串 'Time(s)' (秒)。
        self.radioButtonGroups['TimeUnit'][2][57] = 'Time(s)'
        
        # 定义一个 AFXStringKeyword，用于存储输入数据的文件名。
        # 'InputData.xls': 默认值。
        self.InputDataNameKw = AFXStringKeyword(self.cmd, 'InputDataName', True, 'InputData.xls')
        # 定义一个 AFXStringKeyword，默认值为空字符串。
        self.keyword64Kw = AFXStringKeyword(self.cmd, 'keyword64', True, '')
        # 定义一个 AFXStringKeyword，默认值为空字符串。
        self.keyword68Kw = AFXStringKeyword(self.cmd, 'keyword68', True, '')
        # 定义一个 AFXStringKeyword，可能用于存储一系列组名和它们的状态（例如 'Steady'）。
        # 'G5,Steady,G6,Steady,G13,Steady,G9': 默认值。
        self.keyword65Kw = AFXStringKeyword(self.cmd, 'keyword65', True, 'G5,Steady,G6,Steady,G13,Steady,G9')
        # 定义一个 AFXStringKeyword，可能用于存储某种操作模式或阶段。
        # 'HOLDING': 默认值。
        self.keyword69Kw = AFXStringKeyword(self.cmd, 'keyword69', True, 'HOLDING')
        # 定义一个 AFXIntKeyword，用于存储整数值。
        # 15: 默认值。
        self.keyword77Kw = AFXIntKeyword(self.cmd, 'keyword77', True, 15)
        # 定义一个 AFXFloatKeyword，用于存储浮点数值。
        # 1000: 默认值。
        self.keyword94Kw = AFXFloatKeyword(self.cmd, 'keyword94', True, 1000)
        
        # 以下代码块用于设置名为 'HFrame33' 的单选按钮组。
        # 检查 'HFrame33' 组是否已存在。
        if not self.radioButtonGroups.has_key('HFrame33'):
            # 如果不存在，则创建相关关键字。
            self.HFrame33Kw1 = AFXIntKeyword(None, 'HFrame33Dummy', True)
            self.HFrame33Kw2 = AFXStringKeyword(self.cmd, 'HFrame33', True)
            self.radioButtonGroups['HFrame33'] = (self.HFrame33Kw1, self.HFrame33Kw2, {})
        # 将内部 ID 58 映射到字符串 'NEW'。
        self.radioButtonGroups['HFrame33'][2][58] = 'NEW'
        # 再次检查 'HFrame33' 组是否已存在。
        # 这种重复检查模式在自动生成的 GUI 代码中很常见，确保即使在不同代码路径下也能正确初始化。
        if not self.radioButtonGroups.has_key('HFrame33'):
            # 如果不存在，则再次创建关键字。
            self.HFrame33Kw1 = AFXIntKeyword(None, 'HFrame33Dummy', True)
            self.HFrame33Kw2 = AFXStringKeyword(self.cmd, 'HFrame33', True)
            self.radioButtonGroups['HFrame33'] = (self.HFrame33Kw1, self.HFrame33Kw2, {})
        # 将内部 ID 59 映射到字符串 'REPLACE'。
        self.radioButtonGroups['HFrame33'][2][59] = 'REPLACE'
        # 设置 HFrame33Kw1 的默认值为 59，即默认选择 'REPLACE'。
        self.HFrame33Kw1.setValue(59)
        
        # 定义一个 AFXStringKeyword，可能用于存储某种自动化设置或模式。
        # 'AUTO': 默认值。
        self.keyword90Kw = AFXStringKeyword(self.cmd, 'keyword90', True, 'AUTO')
        
        # 以下代码块用于设置名为 'GroupBox23' 的单选按钮组。
        # 检查 'GroupBox23' 组是否已存在。
        if not self.radioButtonGroups.has_key('GroupBox23'):
            # 如果不存在，则创建相关关键字。
            self.GroupBox23Kw1 = AFXIntKeyword(None, 'GroupBox23Dummy', True)
            # 创建一个 AFXStringKeyword，用于存储选定的字符串值，并将其与命令关联。
            self.GroupBox23Kw2 = AFXStringKeyword(self.cmd, 'GroupBox23', True)
            # 将新创建的关键字和映射字典添加到单选按钮组字典中。
            self.radioButtonGroups['GroupBox23'] = (self.GroupBox23Kw1, self.GroupBox23Kw2, {})
        self.radioButtonGroups['GroupBox23'][2][60] = '\xa1\xa1'
        self.GroupBox23Kw1.setValue(60)
        # 'Steady': 默认值。
        self.keyword72Kw = AFXStringKeyword(self.cmd, 'keyword72', True, 'Steady')
# 下面代码定义了插件的关键字 (Keywords) 和一些核心方法，用于与 Abaqus GUI 交互。
# Keywords 用于存储用户在对话框中输入的数据，并在插件执行时传递给 Abaqus Kernel。
            # 初始化一个单选按钮组 'GroupBox23'。
            # 这个元组包含三个元素：
            # 1. GroupBox23Kw1 (AFXIntKeyword): 用于存储用户选择的单选按钮的内部整数 ID。
            # 2. GroupBox23Kw2 (AFXStringKeyword): 用于存储与所选 ID 对应的字符串值，最终会传递给命令。
            # 3. {}: 一个字典，用于映射内部 ID 到其对应的字符串值。
        # 再次检查 'GroupBox23' 组是否已存在。
        # 这种重复检查可能是在代码重构或模块化过程中产生的，确保无论代码执行路径如何，该组都被正确初始化。
        if not self.radioButtonGroups.has_key('GroupBox23'):
            # 如果不存在，则重新创建 AFXIntKeyword 和 AFXStringKeyword 实例。
            # 注意 GroupBox23Kw1 在这里使用 None 作为第一个参数，表示它不直接关联到命令，
            # 而是作为内部状态管理。
            self.GroupBox23Kw1 = AFXIntKeyword(None, 'GroupBox23Dummy', True)
            # GroupBox23Kw2 仍然关联到命令。
            self.GroupBox23Kw2 = AFXStringKeyword(self.cmd, 'GroupBox23', True)
            # 重新初始化单选按钮组的元组和映射字典。
            self.radioButtonGroups['GroupBox23'] = (self.GroupBox23Kw1, self.GroupBox23Kw2, {})
        # 将内部 ID 61 映射到字符串 'or'。
        # 这表示在 GUI 中，如果用户选择了对应 ID 61 的单选按钮，则其值将是 'or'。
        self.radioButtonGroups['GroupBox23'][2][61] = 'or'
        
        # 定义一个 AFXStringKeyword，默认值为空字符串。
        # 这个关键字可能用于存储用户输入的文本信息，或者作为其他选项的占位符。
        self.keyword73Kw = AFXStringKeyword(self.cmd, 'keyword73', True, '')
        # 定义一个 AFXStringKeyword，默认值为空字符串。
        # 这个关键字可能用于存储一个通用字符串值，其具体用途取决于插件的逻辑。
        self.keyword70Kw = AFXStringKeyword(self.cmd, 'keyword70', True)
        # 定义一个 AFXStringKeyword，默认值为空字符串。
        # 类似于 keyword73Kw，用于存储文本或选项。
        self.keyword74Kw = AFXStringKeyword(self.cmd, 'keyword74', True, '')
        # 定义一个 AFXStringKeyword，默认值为空字符串。
        # 另一个通用字符串关键字，可能用于存储特定配置或用户输入。
        self.keyword92Kw = AFXStringKeyword(self.cmd, 'keyword92', True, '')
        # 定义一个 AFXStringKeyword，与 keyword65Kw 类似，可能用于存储组名和状态。
        # 'G5,Steady,G6,Steady,G13,Steady,G9': 默认值。
        # 这个默认值看起来像是一个逗号分隔的列表，表示多个组及其状态（例如，G5 处于 Steady 状态）。
        self.keyword80Kw = AFXStringKeyword(self.cmd, 'keyword80', True, 'G5,Steady,G6,Steady,G13,Steady,G9')
        # 定义一个 AFXStringKeyword，默认值为空字符串。
        # 用于存储额外的字符串数据。
        self.keyword93Kw = AFXStringKeyword(self.cmd, 'keyword93', True, '')
        # 定义一个 AFXIntKeyword，用于存储整数值。
        # 15: 默认值。注意，这个关键字的名称 ('keyword77') 与之前的 keyword77Kw 相同，但实例名不同。
        # 这个关键字可能用于控制某个整数参数，例如迭代次数或某个选项的索引。
        self.keyword81Kw = AFXIntKeyword(self.cmd, 'keyword77', True, 15)
        # 定义一个 AFXTableKeyword，用于存储表格数据。
        # AFXTableKeyword 允许插件处理多行多列的结构化数据。
        self.keyword78Kw = AFXTableKeyword(self.cmd, 'keyword78', True)
        # 设置 keyword78Kw 表格的第一列为字符串类型。
        self.keyword78Kw.setColumnType(0, AFXTABLE_TYPE_STRING)
        # 设置 keyword78Kw 表格的第二列为字符串类型。
        self.keyword78Kw.setColumnType(1, AFXTABLE_TYPE_STRING)
        # 设置 keyword78Kw 表格的第三列为字符串类型。
        self.keyword78Kw.setColumnType(2, AFXTABLE_TYPE_STRING)
        # 定义一个 AFXTableKeyword，用于存储表格数据。
        # 这是另一个表格关键字，可能用于存储不同类型的数据。
        self.keyword82Kw = AFXTableKeyword(self.cmd, 'keyword82', True)
        # 设置 keyword82Kw 表格的第一列为浮点数类型。
        self.keyword82Kw.setColumnType(0, AFXTABLE_TYPE_FLOAT)
        # 设置 keyword82Kw 表格的第二列为浮点数类型。
        self.keyword82Kw.setColumnType(1, AFXTABLE_TYPE_FLOAT)
        # 定义一个 AFXTableKeyword，用于存储表格数据。
        # 第三个表格关键字，用于存储更多结构化数据。
        self.keyword83Kw = AFXTableKeyword(self.cmd, 'keyword83', True)
        # 设置 keyword83Kw 表格的第一列为浮点数类型。
        self.keyword83Kw.setColumnType(0, AFXTABLE_TYPE_FLOAT)
        # 设置 keyword83Kw 表格的第二列为浮点数类型。
        self.keyword83Kw.setColumnType(1, AFXTABLE_TYPE_FLOAT)
        # 设置 keyword83Kw 表格的第三列为浮点数类型。
        self.keyword83Kw.setColumnType(2, AFXTABLE_TYPE_FLOAT)
        # 设置 keyword83Kw 表格的第四列为浮点数类型。
        self.keyword83Kw.setColumnType(3, AFXTABLE_TYPE_FLOAT)
        # 设置 keyword83Kw 表格的第五列为浮点数类型。
        self.keyword83Kw.setColumnType(4, AFXTABLE_TYPE_FLOAT)
        # 定义一个 AFXStringKeyword，用于存储用户指定的子程序名称。
        # 默认值为空字符串。
        # 这个关键字可能用于指定一个用户自定义的 Fortran 或 C++ 子程序，供 Abaqus 调用。
        self.SubroutineNameKw = AFXStringKeyword(self.cmd, 'SubroutineName', True, '')
        # 定义一个 AFXStringKeyword，可能用于存储某种计算方法或代码。
        # 'RCC': 默认值。
        # 'RCC' 可能代表某种特定的计算方法、算法或模型类型。
        self.keyword43Kw = AFXStringKeyword(self.cmd, 'keyword43', True, 'RCC')
        # 定义一个 AFXTableKeyword，用于存储表格数据。
        # 这是第四个表格关键字，用于存储与插件功能相关的数值数据。
        self.keyword16Kw = AFXTableKeyword(self.cmd, 'keyword16', True)
        # 设置 keyword16Kw 表格的第一列到第七列为浮点数类型。
        # 这表明该表格可能用于存储一组七个浮点数，例如坐标、材料属性或时间序列数据。
        self.keyword16Kw.setColumnType(0, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(1, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(2, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(3, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(4, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(5, AFXTABLE_TYPE_FLOAT)
        self.keyword16Kw.setColumnType(6, AFXTABLE_TYPE_FLOAT)
        # 定义一个 AFXTableKeyword，用于存储表格数据。
        # 第五个表格关键字。
        self.keyword35Kw = AFXTableKeyword(self.cmd, 'keyword35', True)
        # 设置 keyword35Kw 表格的第一列到第四列为浮点数类型。
        # 可能用于存储四列浮点数数据。
        self.keyword35Kw.setColumnType(0, AFXTABLE_TYPE_FLOAT)
        self.keyword35Kw.setColumnType(1, AFXTABLE_TYPE_FLOAT)
        self.keyword35Kw.setColumnType(2, AFXTABLE_TYPE_FLOAT)
        self.keyword35Kw.setColumnType(3, AFXTABLE_TYPE_FLOAT)
        # 定义一个 AFXTableKeyword，用于存储表格数据。
        # 第六个表格关键字。
        self.keyword41Kw = AFXTableKeyword(self.cmd, 'keyword41', True)
        # 设置 keyword41Kw 表格的第一列到第四列为浮点数类型。
        # 同样用于存储四列浮点数数据。
        self.keyword41Kw.setColumnType(0, AFXTABLE_TYPE_FLOAT)
        self.keyword41Kw.setColumnType(1, AFXTABLE_TYPE_FLOAT)
        self.keyword41Kw.setColumnType(2, AFXTABLE_TYPE_FLOAT)
        self.keyword41Kw.setColumnType(3, AFXTABLE_TYPE_FLOAT)
        # 定义一个 AFXIntKeyword，用于存储整数值。
        # 3: 默认值。
        # 这个关键字可能用于控制某个计数器、索引或选项。
        self.keyword08Kw = AFXIntKeyword(self.cmd, 'keyword08', True, 3)
        # 定义另一个 AFXIntKeyword，用于存储整数值。
        # 3: 默认值。
        # 另一个整数关键字，可能与 keyword08Kw 协同工作或控制不同的参数。
        self.keyword07Kw = AFXIntKeyword(self.cmd, 'keyword07', True, 3)
        # 定义一个 AFXFloatKeyword，用于存储浮点数值。
        # 此关键字没有默认值，意味着用户必须在 GUI 中输入，或者在命令执行前由程序设置。
        # 这是一个强制用户输入的浮点数参数。
        self.keyword30Kw = AFXFloatKeyword(self.cmd, 'keyword30', True)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 此方法返回插件的第一个对话框实例。
    # Abaqus GUI 插件通常将对话框的实际布局和控件定义在单独的模块中。
    # 这是 Abaqus GUI 插件框架中的一个标准方法，用于获取插件的用户界面。
    def getFirstDialog(self):

        # 导入 sTPM_test1033DB 模块。这个模块很可能包含了定义 GUI 布局的类。
        # sTPM_test1033DB 模块通常包含 AFXDialog 或其子类的定义，用于构建对话框的视觉元素。
        import sTPM_test1033DB
        # 重新加载 sTPM_test1033DB 模块。这在开发过程中非常有用，可以确保每次打开对话框时都加载最新的代码更改。
        # 在生产环境中，通常不需要 reload，但在开发和调试插件时非常方便，可以避免重启 Abaqus。
        reload(sTPM_test1033DB)
        # 返回 sTPM_test1033DB 模块中定义的 STPM_test1033DB 类的一个实例。
        # 将当前插件实例 (self) 作为对话框的所有者传递。
        # 这样，对话框可以访问插件实例的关键字和其他属性。
        return sTPM_test1033DB.STPM_test1033DB(self)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 此方法在表单被提交（例如，用户点击“确定”按钮）之前执行自定义检查。
    # 这是插件在用户点击“确定”或“应用”按钮后，但在实际命令执行前，进行数据验证和预处理的关键点。
    def doCustomChecks(self):

        # 遍历 self.radioButtonGroups 字典中的所有单选按钮组。
        # kw1: 对应于 AFXIntKeyword，存储用户选择的内部 ID。
        # kw2: 对应于 AFXStringKeyword，存储将传递给命令的实际字符串值。
        # d: 存储 ID 到字符串值映射的字典。
        # 这种遍历确保所有单选按钮组的内部整数 ID 都被转换为其对应的字符串值。
        for kw1,kw2,d in self.radioButtonGroups.values():
            # 使用 try-except 块来处理可能出现的错误，例如用户没有选择任何选项。
            try:
                # 尝试根据 AFXIntKeyword (kw1) 的当前值，从映射字典 (d) 中获取对应的字符串值。
                # 这是将 GUI 内部表示（整数 ID）转换为 Abaqus Kernel 命令所需字符串的关键步骤。
                value = d[ kw1.getValue() ]
                # 将获取到的字符串值设置给 AFXStringKeyword (kw2)。
                # 这样，当 AFXGuiCommand 执行时，它会接收到正确的字符串值，而不是内部 ID。
                kw2.setValue(value)
            except:
                # 如果在获取值时发生异常（例如，kw1.getValue() 返回了一个在 d 中不存在的键，
                # 或者 kw1 根本没有值），则捕获异常并忽略。
                # 这意味着如果用户没有选择任何单选按钮，或者存在配置问题，程序不会崩溃。
                # 这种处理方式使得插件在面对不完整或异常的用户输入时更加健壮。
                pass
        # 返回 True 表示所有自定义检查都已通过，表单可以继续提交。
        # 如果返回 False，则表单提交将被阻止，并且通常会显示一个错误消息给用户。
        return True

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 此方法决定在某些操作（如文件新建、打开或模型更改）发生时，对话框是否可以自动关闭。
    # 这是 Abaqus GUI 插件生命周期管理的一部分。
    def okToCancel(self):

        # 返回 False 表示在执行文件操作（如新建或打开文件）或模型更改时，
        # 此对话框不应自动关闭。这允许用户在不关闭插件对话框的情况下进行其他 Abaqus 操作。
        # 这对于需要用户在插件对话框打开的同时进行其他 Abaqus 操作（如选择模型中的实体）的插件非常有用。
        return False

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 注册 Abaqus 插件到 GUI。
# 这部分代码负责将插件集成到 Abaqus 的用户界面中，使其可以通过菜单或工具栏访问。
# 导入 os 模块，用于处理文件路径。
import os
# 获取当前脚本文件的绝对路径。
thisPath = os.path.abspath(__file__)
# 从脚本的绝对路径中提取其所在的目录路径。
# 这通常用于定位插件相关的其他文件，如图标或数据文件。
thisDir = os.path.dirname(thisPath)

# 获取 Abaqus 应用程序的主窗口实例，并从中获取插件工具集。
# getAFXApp() 获取当前 Abaqus 应用程序实例。
# getAFXMainWindow() 获取主窗口。
# getPluginToolset() 获取用于管理插件的工具集。
toolset = getAFXApp().getAFXMainWindow().getPluginToolset()
# 向插件工具集注册一个 GUI 菜单按钮。这将使插件在 Abaqus GUI 中可见并可交互。
# registerGuiMenuButton 是 Abaqus GUI 插件注册的主要函数。
toolset.registerGuiMenuButton(
    # 按钮上显示的文本。'非弹工具|非弹性建模（前处理）' 表示它将出现在名为“非弹工具”的菜单下，
    # 按钮本身的文本是“非弹性建模（前处理）”。.encode('GB18030') 用于正确处理中文字符编码。
    # 使用 u'' 前缀表示 Unicode 字符串，然后编码为 GB18030 以确保在某些系统上正确显示中文。
    buttonText=u'非弹工具|非弹性建模（前处理）'.encode('GB18030'), 
    # 当按钮被点击时，将激活这个插件实例。
    # 这里创建了 STPM_test1033_plugin 类的一个实例，并将其作为 object 参数传递。
    object=STPM_test1033_plugin(toolset),
    # 消息ID，指示点击按钮时应激活插件的表单。
    # AFXMode.ID_ACTIVATE 是一个标准消息 ID，用于激活插件的 GUI。
    messageId=AFXMode.ID_ACTIVATE,
    # 按钮图标。设置为 None 表示不使用自定义图标。
    # 可以通过提供图标文件的路径来设置自定义图标。
    icon=None,
    # 当插件被激活时，在 Abaqus Kernel 中执行的初始化字符串。
    # 'import ECUST' 确保 ECUST 模块在 Kernel 命名空间中可用，以便后续可以调用其中的方法（如 fun1）。
    # 这允许插件在 Abaqus Kernel 中预加载必要的模块或执行初始化命令。
    kernelInitString='import ECUST',
    # 插件适用的 Abaqus 模块。ALL 表示此插件可以在所有 Abaqus 模块中使用。
    # 例如，可以指定为 [MODEL, JOB] 等，限制插件在特定模块中可用。
    applicableModules=ALL,
    # 插件的版本信息。
    version='N/A',
    # 插件的作者信息。
    author='N/A',
    # 插件的简短描述。
    description='N/A',
    # 插件帮助文档的 URL。
    helpUrl='N/A'
)


