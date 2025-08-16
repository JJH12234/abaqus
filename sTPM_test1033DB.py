# -*- coding: utf-8 -*-
from abaqusConstants import *
# 从 abaqusConstants 模块导入所有常量，这些常量通常用于定义Abaqus中的各种选项和行为。
from abaqusGui import *
# 从 abaqusGui 模块导入所有GUI相关的类和函数，用于构建Abaqus插件的用户界面。
from kernelAccess import mdb, session
# 从 kernelAccess 模块导入 mdb 和 session 对象，mdb 用于访问Abaqus模型数据库，session 用于访问当前会话。
import os
# 导入 os 模块，用于操作系统相关的操作，如路径处理。
import json
# 导入 json 模块，用于处理JSON数据，例如读取或写入JSON文件。
import xlrd
# 导入 xlrd 模块，用于读取Excel文件（.xls格式）。
import traceback
# 导入 traceback 模块，用于打印异常的堆栈跟踪信息，便于调试。
from collections import OrderedDict, Counter, defaultdict
# 从 collections 模块导入 OrderedDict, Counter, defaultdict，提供额外的数据结构。
# OrderedDict 保持插入顺序的字典。
# Counter 用于计数可哈希对象。
# defaultdict 用于在访问不存在的键时提供默认值。
PLUGIN_DIR = os.path.dirname(os.path.abspath(__file__))
# 获取当前脚本文件所在的目录，并将其赋值给 PLUGIN_DIR 变量。
# os.path.abspath(__file__) 获取当前脚本的绝对路径。
# os.path.dirname() 获取路径中的目录部分。
# os.chdir(PLUGIN_DIR)
# 这一行被注释掉了，如果启用，它会将当前工作目录更改为插件目录。
import re
# 导入 re 模块，用于正则表达式操作。
thisPath = os.path.abspath(__file__)
# 获取当前脚本文件的绝对路径。
thisDir = os.path.dirname(thisPath)
# 获取当前脚本文件所在的目录。


###########################################################################
# Class definition
# 类定义部分
###########################################################################

class STPM_test1033DB(AFXDataDialog):
    # 定义一个名为 STPM_test1033DB 的类，它继承自 AFXDataDialog。
    # AFXDataDialog 是 Abaqus GUI 工具包中用于创建数据输入对话框的基类。
    [
        ID_CLICKED_IMPORT1,
        # 定义一个ID常量，用于导入操作的点击事件。
        ID_MODEL_MATERIAL_COMBO_CHANGED,
        # 定义一个ID常量，用于模型材料组合框选择改变事件。
        ID_CLICKED_NEW,
        # 定义一个ID常量，用于新建操作的点击事件。
        ID_CLICKED,
        # 定义一个通用ID常量，用于各种点击事件。
        ID_COMBO_CHANGED_SHEET,
        # 定义一个ID常量，用于工作表组合框选择改变事件。
        ID_COMBO_CHANGED_TREE,
        # 定义一个ID常量，用于树形控件组合框选择改变事件。
        ID_FILE_CHANGED,
        # 定义一个ID常量，用于文件改变事件。
        ID_FILE_NAME_CHANGED,
        # 定义一个ID常量，用于文件名改变事件。
        ID_FILE_NAME_CHANGED_1,
        # 定义另一个ID常量，用于文件名改变事件。
        ID_CLICKED_LIST,
        # 定义一个ID常量， 用于列表控件的点击事件。
        ID_TEXT_CHANGED,
        # 定义一个ID常量， 用于文本框内容改变事件。
        ID_CYCLE_LIST_CHANGED,
        # 定义一个ID常量， 用于循环列表改变事件。
        ID_TAB_CHANGED,
        # 定义一个ID常量， 用于标签页改变事件。
        ID_TAB5_CHANGED,
        # 定义一个ID常量， 用于特定标签页（Tab5）改变事件。
        ID_CLICKED_IMPORTFUZHI,
        # 定义一个ID常量， 用于“导入复制”操作的点击事件。
        ID_CLICKED_CREATESTEP,
        # 定义一个ID常量， 用于“创建步”操作的点击事件。
        ID_CLICKED_MODIFYSTEP,
        # 定义一个ID常量， 用于“修改步”操作的点击事件。
        ID_CLICKED_UPDATEMODEL,
        # 定义一个ID常量， 用于“更新模型”操作的点击事件。
        ID_CLICKED_AMPPAIR,
        # 定义一个ID常量， 用于“AMPPAIR”操作的点击事件。
        ID_PARAMNAME_DBLCLICK
    ] = range(AFXForm.ID_LAST+1, AFXForm.ID_LAST + 21)
    # 这些ID常量被赋值为从 AFXForm.ID_LAST+1 开始的连续整数，用于唯一标识GUI事件。
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def __init__(self, form):
        # 类的构造函数，在创建 STPM_test1033DB 实例时被调用。
        # form 参数通常是 AFXForm 类的实例，用于管理对话框中的数据。

        # Construct the base class.
        # 调用基类 AFXDataDialog 的构造函数。
        mw = getAFXApp().getAFXMainWindow()
        # 获取 Abaqus 应用程序的主窗口对象。
        AFXDataDialog.__init__(self, form, 'STPM_GB_Before',)
                               # self.OK | self.CANCEL, )
        # 初始化 AFXDataDialog 基类，设置对话框的父级、标题。
        # 注释掉的部分 表示对话框原本可以有OK/CANCEL按钮，但当前配置中未启用。
        self.materials_data = {}
        # 初始化一个空字典，用于存储材料数据。
        self.steptimepair = {}
        # 初始化一个空字典， 用于存储步长和时间对数据。
        self.form = form
        # 将传入的 form 对象存储为实例属性。
        self.temp_json = {}  # 添加临时JSON存储
        # 初始化一个空字典，用于临时存储JSON数据。
        self.tab34flag=False
        # 初始化一个布尔标志， 用于控制标签页3和4的显示或行为。
        # okBtn = self.getActionButton(self.ID_CLICKED_OK)
        # 这行代码被注释掉了，如果启用，它会尝试获取一个ID为 ID_CLICKED_OK 的动作按钮。
        # okBtn.setText('OK')
        # 这行代码被注释掉了，如果启用，它会设置该按钮的文本为'OK'。

        TabBook_1 = FXTabBook(p=self, tgt=None, sel=0,
                              opts=TABBOOK_NORMAL | LAYOUT_FILL_X | LAYOUT_FILL_Y,
                              x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                              pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)
        # 创建一个 FXTabBook 控件，这是一个标签页容器。
        # p=self: 父控件是当前对话框。
        # tgt=None: 没有目标对象（事件处理由子控件处理）。
        # sel=0: 默认选中第一个标签页。
        # opts: 控件选项，包括正常标签页样式、填充父控件的水平和垂直空间。
        # x, y, w, h: 控件的初始位置和大小（通常由布局管理器控制）。
        # pl, pr, pt, pb: 内边距设置。

        # try:
        #     excel_path = u"D:/桌面/工程开发项目/ParaModelingData.xls"
        #     workbook = xlrd.open_workbook(excel_path)
        #     sheet = workbook.sheet_by_index(0)
        #
        #     self.excel_data = []
        #     for row in range(sheet.nrows):
        #         row_data = []
        #         for col in range(sheet.ncols):
        #             value = sheet.cell_value(row, col)
        #             row_data.append(value)
        #         self.excel_data.append(row_data)
        # except Exception as e:
        #     mw = getAFXApp().getAFXMainWindow()
        #     mw.writeToMessageArea("Error reading Excel file:" + str(e))
        #     self.excel_data = None
        # 这段被注释掉的代码块尝试读取一个Excel文件。
        # 它会打开指定路径的Excel文件，读取第一个工作表的所有行和列数据，并存储在 self.excel_data 中。
        # 如果读取过程中发生错误，会将错误信息写入Abaqus消息区域。

        tabItem = FXTabItem(p=TabBook_1, text=u'几何模型'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        # 创建一个 FXTabItem，作为 TabBook_1 的一个标签页。
        # text: 标签页的显示文本，使用GB18030编码显示中文“几何模型”。
        # ic=None: 没有图标。
        # opts: 标签页选项，顶部正常样式。
        # pl, pr, pt, pb: 内边距。
        TabItem_22 = FXVerticalFrame(p=TabBook_1,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # 创建一个 FXVerticalFrame 控件，作为 TabBook_1 的内容面板，用于放置“几何模型”标签页内的控件。
        # opts: 框架选项，包括凸起边框、粗边框、填充水平空间。
        # pl, pr, pt, pb: 内边距。
        # hs, vs: 水平和垂直间距。
        fileHandler = XslFileHandler(form, self, 'fileName', '(*.xls)')
        # 创建一个 XslFileHandler 实例，用于处理Excel文件选择。
        # 它将与 form、当前对话框实例、文件名关键字 'fileName' 和文件过滤器 '(*.xls)' 关联。
        fileTextHf = FXHorizontalFrame(p=TabItem_22, opts=0, x=0, y=0, w=0, h=0,
                                       pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # 创建一个 FXHorizontalFrame 控件，用于水平布局文件选择相关的文本框和按钮。
        # p=TabItem_22: 父控件是 TabItem_22。
        # opts=0: 无特殊选项。
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        # 这段注释是RSG Dialog Builder工具自动生成的，提示不要为这个控件设置特殊的颜色选择器，以避免与布局管理器冲突。
        fileTextHf.setSelector(99)
        # 设置 fileTextHf 的选择器ID为99。
        AFXTextField(p=fileTextHf, ncols=60, labelText='File name:', tgt=form.fileNameKw, sel=0,
                     opts=AFXTEXTFIELD_STRING | LAYOUT_CENTER_Y)
        # 创建一个 AFXTextField 控件，用于显示或输入文件路径。
        # p=fileTextHf: 父控件是 fileTextHf。
        # ncols=60: 文本框的列宽。
        # labelText: 文本框前的标签文本。
        # tgt=form.fileNameKw: 目标关键字，将文本框的值绑定到 form 对象的 fileNameKw 属性。
        # sel=0: 默认选择器。
        # opts: 选项，表示这是一个字符串文本框，并垂直居中。
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL)
        # 获取一个预定义的“文件打开”图标，小尺寸。
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
                 opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        # 创建一个 FXButton 控件，用于触发文件选择对话框。
        # text: 按钮上显示的文本。
        # ic=icon: 按钮上显示的图标。
        # tgt=fileHandler: 按钮点击事件的目标对象是 fileHandler。
        # sel=AFXMode.ID_ACTIVATE: 按钮激活时触发的事件选择器。
        # opts: 按钮选项，包括正常样式和垂直居中。
        # pl, pr, pt, pb: 内边距。
        self.ComboBox_14 = AFXComboBox(p=TabItem_22, ncols=0, nvis=1, text='Model:', tgt=form.keyword95Kw, sel=0)
        # 创建一个 AFXComboBox 控件（下拉列表），用于选择模型。
        # p=TabItem_22: 父控件是 TabItem_22。
        # ncols=0: 列数（通常由内容决定）。
        # nvis=1: 可见项数。
        # text: 组合框前的标签文本。
        # tgt=form.keyword95Kw: 目标关键字，将选中的值绑定到 form 对象的 keyword95Kw 属性。
        self.ComboBox_14.setMaxVisible(10)
        # 设置组合框下拉时最多显示10个项目。
        self.ComboBox_14.appendItem(text='111')
        # 向组合框添加一个项目，文本为'111'。
        self.ComboBox_14.appendItem(text='222')
        # 向组合框添加一个项目，文本为'222'。
        self.ComboBox_14.appendItem(text='...')
        # 向组合框添加一个项目，文本为'...'。
        self.ComboBox_14.setTarget(self)
        # 设置组合框的事件目标为当前对话框实例 (self)。
        self.ComboBox_14.setSelector(self.ID_COMBO_CHANGED_SHEET)
        # 设置组合框选择改变时触发的事件选择器为 ID_COMBO_CHANGED_SHEET。
        FXMAPFUNC(self, SEL_COMMAND, self.ID_COMBO_CHANGED_SHEET, self.onSheetChanged)
        # 将 ID_COMBO_CHANGED_SHEET 事件（当组合框选择改变时发送的命令）映射到 onSheetChanged 方法。
        # SEL_COMMAND: 表示这是一个命令事件。

        vf = FXVerticalFrame(TabItem_22, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # 创建一个 FXVerticalFrame 控件，用于垂直布局表格。
        # p=TabItem_22: 父控件是 TabItem_22。
        # opts: 框架选项，包括下沉边框、粗边框、填充水平空间。
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        # RSG Dialog Builder工具自动生成的注释。
        vf.setSelector(99)
        # 设置 vf 的选择器ID为99。
        self.table = AFXTable(vf, 20, 5, 200, 6, form.keyword97Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        # 创建一个 AFXTable 控件（表格）。
        # p=vf: 父控件是 vf。
        # 20: 初始行数。
        # 5: 初始列数。
        # 200: 初始宽度。
        # 6: 初始高度。
        # form.keyword97Kw: 绑定到 form 对象的 keyword97Kw 属性。
        # 0: 默认选择器。
        # opts: 表格选项，包括可编辑和填充水平空间。
        self.table.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        # 设置表格的右键弹出菜单选项，包括剪切、复制、粘贴、插入行、删除行、清除内容、从文件读取、写入文件。
        self.table.setLeadingRows(1)
        # 设置表格的引导行数（通常用于表头）。
        self.table.setLeadingColumns(1)
        # 设置表格的引导列数（通常用于行号或行标签）。
        self.table.setColumnWidth(1, 120)
        # 设置第一列的宽度为120像素。
        self.table.setColumnType(1, AFXTable.TEXT)
        # 设置第一列的类型为文本。
        self.table.setColumnWidth(2, 80)
        # 设置第二列的宽度为80像素。
        self.table.setColumnType(2, AFXTable.TEXT)
        # 设置第二列的类型为文本。
        self.table.setColumnWidth(3, 150)
        # 设置第三列的宽度为150像素。
        self.table.setColumnType(3, AFXTable.TEXT)
        # 设置第三列的类型为文本。
        self.table.setColumnWidth(4, 100)
        # 设置第四列的宽度为100像素。
        self.table.setColumnType(4, AFXTable.TEXT)
        # 设置第四列的类型为文本。
        self.table.setColumnWidth(5, 80)
        # 设置第五列的宽度为80像素。
        self.table.setColumnType(5, AFXTable.TEXT)
        # 设置第五列的类型为文本。
        self.table.setLeadingRowLabels(u'参数名\t参数值\t类型\t部件\t特征'.encode('GB18030'))
        # 设置表格引导行的标签，使用制表符分隔，并用GB18030编码显示中文。
        self.table.setStretchableColumn(self.table.getNumColumns() - 1)
        # 设置最后一列为可伸缩列，使其在表格大小改变时自动调整宽度。
        self.table.showHorizontalGrid(True)
        # 显示表格的水平网格线。
        self.table.showVerticalGrid(True)
        self.table.setTarget(self)
        self.table.setColumnEditable(1, False)   # 参数名 只读
        self.table.setColumnEditable(2, True)    # 参数值 可编辑
        self.table.setColumnEditable(3, False)   # 类型 只读
        self.table.setColumnEditable(4, False)   # 部件 只读
        self.table.setColumnEditable(5, False)   # 特征 只读
        self.table.shadeReadOnlyItems(True)      # 只读单元格加阴影（可选）. :contentReference[oaicite:6]{index=6}
        self.table.setTarget(self)
        self.table.setSelector(self.ID_PARAMNAME_DBLCLICK)
        FXMAPFUNC(self, SEL_DOUBLECLICKED, self.ID_PARAMNAME_DBLCLICK,
          STPM_test1033DB.onParamNameDblClicked)
        # 显示表格的垂直网格线。

        # if self.excel_data:
        #     try:
        #
        #         for i in range(0, len(self.excel_data)):
        #             for j in range(0, len(self.excel_data[i])):
        #                 table.setItemText(i + 1, j + 1, str(self.excel_data[i][j]))
        #     except Exception as e:
        #         print("Error filling table:", str(e))
        # 这段被注释掉的代码块用于将之前读取的Excel数据填充到表格中。
        # 它会遍历 self.excel_data，并将每个单元格的值设置到表格的对应位置。

        fileName = os.path.join(thisDir, 'icon.png')
        # 构建图标文件的完整路径。
        icon = afxCreatePNGIcon(fileName)
        # 从PNG文件创建图标对象。
        FXLabel(p=TabItem_22, text='', ic=icon)
        # 创建一个 FXLabel 控件，显示图标。
        # p=TabItem_22: 父控件是 TabItem_22。
        # text='': 标签没有文本。
        # ic=icon: 显示指定的图标。
        fileName = os.path.join(thisDir, 'icon.png')
        # 再次构建图标文件的完整路径。
        icon = afxCreatePNGIcon(fileName)
        # 再次从PNG文件创建图标对象。
        FXLabel(p=TabItem_22, text='', ic=icon)
        # 再次创建一个 FXLabel 控件，显示图标。
        fileName = os.path.join(thisDir, 'icon.png')
        # 第三次构建图标文件的完整路径。
        icon = afxCreatePNGIcon(fileName)
        # 第三次从PNG文件创建图标对象。
        FXLabel(p=TabItem_22, text='', ic=icon)
        # 第三次创建一个 FXLabel 控件，显示图标。
        updateBtn = FXButton(p=TabItem_22,
                     text=u'更新模型尺寸'.encode('GB18030'),
                     ic=None,
                     tgt=self,
                     sel=self.ID_CLICKED_UPDATEMODEL,
                     opts=BUTTON_NORMAL | JUSTIFY_LEFT)
        # 创建一个 FXButton 控件，用于“更新模型尺寸”操作。
        # text: 按钮文本，使用GB18030编码显示中文。
        # ic=None: 没有图标。
        # tgt=self: 按钮点击事件的目标对象是当前对话框实例。
        # sel=self.ID_CLICKED_UPDATEMODEL: 按钮点击时触发的事件选择器。
        # opts: 按钮选项，包括正常样式和左对齐文本。
        updateBtn.setTarget(self)
        # 再次设置按钮的事件目标为当前对话框实例。
        updateBtn.setSelector(self.ID_CLICKED_UPDATEMODEL)
        # 再次设置按钮的事件选择器。
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_UPDATEMODEL, STPM_test1033DB.onUpdateModelClicked)
        # 将 ID_CLICKED_UPDATEMODEL 事件（当按钮点击时发送的命令）映射到 STPM_test1033DB 类的 onUpdateModelClicked 方法。
        # button = FXButton(p=TabItem_22, text=u'更新模型尺寸'.encode('GB18030'), opts=BUTTON_NORMAL | JUSTIFY_LEFT)
        # 这行被注释掉的代码是另一个创建“更新模型尺寸”按钮的示例。
        tabItem = FXTabItem(p=TabBook_1, text=u'材料'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        # 创建第二个 FXTabItem，作为 TabBook_1 的一个标签页，文本为“材料”。
        TabItem_16 = FXVerticalFrame(p=TabBook_1,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # 创建一个 FXVerticalFrame 控件，作为 TabBook_1 的内容面板，用于放置“材料”标签页内的控件。
        fileHandler = TesttreeDBFileHandler(form, self,'JSONName', 'Json files (*.json)')
        # 创建一个 TesttreeDBFileHandler 实例，用于处理JSON文件选择。
        fileTextHf = FXHorizontalFrame(p=TabItem_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        # 创建一个 FXHorizontalFrame 控件，用于水平布局JSON文件选择相关的文本框和按钮。
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        # RSG Dialog Builder工具自动生成的注释。
        fileTextHf.setSelector(99)
        # 设置 fileTextHf 的选择器ID为99。
        AFXTextField(p=fileTextHf, ncols=50, labelText='JSON:', tgt=form.JSONNameKw, sel=0,
            opts=AFXTEXTFIELD_STRING|LAYOUT_CENTER_Y)
        # 创建一个 AFXTextField 控件，用于显示或输入JSON文件路径。
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL )
        # 获取一个预定义的“文件打开”图标。
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
            opts=BUTTON_NORMAL|LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        # 创建一个 FXButton 控件，用于触发JSON文件选择对话框。
        HFrame_26 = FXHorizontalFrame(p=TabItem_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        # 创建一个 FXHorizontalFrame 控件，用于水平布局材料选择和属性显示区域。
        VFrame_24 = FXVerticalFrame(p=HFrame_26, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        # 创建一个 FXVerticalFrame 控件，用于垂直布局材料组合框和树形列表。
        self.ComboBox_12 = AFXComboBox(p=VFrame_24, ncols=0, nvis=1, text='Material', tgt=form.keyword88Kw, sel=0)
        # 创建一个 AFXComboBox 控件（下拉列表），用于选择材料。
        self.ComboBox_12.setMaxVisible(10)
        # 设置组合框下拉时最多显示10个项目。
        self.ComboBox_12.appendItem(text='2.25Cr1Mo')
        # 向组合框添加一个材料项目。
        self.ComboBox_12.appendItem(text='SA336')
        # 向组合框添加一个材料项目。
        self.ComboBox_12.appendItem(text='Na')
        # 向组合框添加一个材料项目。
        self.ComboBox_12.setTarget(self)
        # 设置组合框的事件目标为当前对话框实例。
        self.ComboBox_12.setSelector(self.ID_COMBO_CHANGED_TREE)
        # 设置组合框选择改变时触发的事件选择器为 ID_COMBO_CHANGED_TREE。
        FXMAPFUNC(self, SEL_DOUBLECLICKED, self.ID_CLICKED, STPM_test1033DB.onItemDoubleClicked)
        # 将 ID_CLICKED 事件（双击）映射到 STPM_test1033DB 类的 onItemDoubleClicked 方法。

        selected_material = self.ComboBox_12.getItemText(self.ComboBox_12.getCurrentItem())
        # 获取当前组合框中选中的材料文本。

        self.listVf = FXVerticalFrame(p=VFrame_24, opts=FRAME_SUNKEN|FRAME_THICK, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        # 创建一个 FXVerticalFrame 控件，用于放置树形列表。
        self.listVf.setSelector(99)
        # 设置 listVf 的选择器ID为99。
        self.tree = FXTreeList(self.listVf, 20, tgt=self, sel=self.ID_CLICKED,
                               opts=LAYOUT_FILL_X | LAYOUT_FILL_Y |
                                    TREELIST_SHOWS_BOXES | TREELIST_ROOT_BOXES |
                                    TREELIST_SHOWS_LINES | LAYOUT_FIX_WIDTH |TREELIST_CHECK_BOXES|HSCROLLER_ALWAYS|TREELIST_PROPAGATE_CHECKS,
                               x=0, y=0, w=200, h=0)
        # 创建一个 FXTreeList 控件（树形列表）。
        # p=self.listVf: 父控件是 listVf。
        # 20: 初始可见行数。
        # tgt=self: 事件目标是当前对话框实例。
        # sel=self.ID_CLICKED: 选中项改变时触发的事件选择器。
        # opts: 树形列表选项，包括填充水平垂直空间、显示复选框、显示根节点复选框、显示连接线、固定宽度、始终显示水平滚动条、复选框状态传播。
        self.updateTree(selected_material)
        # 调用 updateTree 方法，根据选中的材料更新树形列表的内容。
        FXMAPFUNC(self, SEL_COMMAND, self.ID_COMBO_CHANGED_TREE, STPM_test1033DB.onMaterialChanged)
        # 将 ID_COMBO_CHANGED_TREE 事件（当材料组合框选择改变时发送的命令）映射到 STPM_test1033DB 类的 onMaterialChanged 方法。
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED, STPM_test1033DB.onTreeCheckChanged)  # 新增事件绑定
        # 将 ID_CLICKED 事件（当树形列表项被点击时发送的命令）映射到 STPM_test1033DB 类的 onTreeCheckChanged 方法。
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED, STPM_test1033DB.get_checked_data)
        # 将 ID_CLICKED 事件（当树形列表项被点击时发送的命令）映射到 STPM_test1033DB 类的 get_checked_data 方法。
        # List_1 = AFXList(p=listVf, nvis=20, tgt=form.keyword47Kw, sel=0, opts=HSCROLLING_OFF|LIST_SINGLESELECT)
        # 这段被注释掉的代码是创建 AFXList 控件（列表）的示例。
        # List_1.appendItem(text='  -Prime property1')
        # List_1.appendItem(text='    -Secondary propetry1')
        # List_1.appendItem(text='       Data1(ASME)')
        # List_1.appendItem(text='       Data2(Test)')
        # List_1.appendItem(text='    +Secondary propetry2')
        # List_1.appendItem(text='\xa1\xa1')
        # List_1.appendItem(text='  +Prime property2')
        # List_1.appendItem(text='  +Prime property3')
        # 这段被注释掉的代码是向 AFXList 控件添加项目的示例。
        if isinstance(HFrame_26, FXHorizontalFrame):
            # 检查 HFrame_26 是否是 FXHorizontalFrame 的实例。
            FXVerticalSeparator(p=HFrame_26, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
            # 如果是，则在 HFrame_26 中添加一个垂直分隔符。
        else:
            FXHorizontalSeparator(p=HFrame_26, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
            # 否则，添加一个水平分隔符。
        VFrame_14 = FXVerticalFrame(p=HFrame_26, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=100, pb=0)
        # 创建一个 FXVerticalFrame 控件，用于垂直布局标签。
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        # 创建一个 FXLabel 控件，显示一个特殊字符（ 是占位符或空白），左对齐。
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        # 再次创建一个 FXLabel 控件。
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        # 再次创建一个 FXLabel 控件。
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        # 再次创建一个 FXLabel 控件。
        HFrame_27 = FXHorizontalFrame(p=VFrame_14, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        # 创建一个 FXHorizontalFrame 控件，用于水平布局。
        VFrame_25 = FXVerticalFrame(p=HFrame_27, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        # 创建一个 FXVerticalFrame 控件，用于垂直布局。
        l = FXLabel(p=VFrame_25, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        # 创建一个 FXLabel 控件。
        l = FXLabel(p=VFrame_25, text='>>', opts=JUSTIFY_LEFT)
        # 创建一个 FXLabel 控件，显示“>>”文本。

        # 检查 HFrame_27 是否是 FXHorizontalFrame 实例
        # 这通常用于根据父容器的类型来决定使用垂直分隔符还是水平分隔符
        if isinstance(HFrame_27, FXHorizontalFrame):
            # 如果是水平框架，则插入一个垂直分隔符
            # p: 父窗口部件 (HFrame_27)
            # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
            # pl, pr, pt, pb: 内边距 (左、右、上、下)
            FXVerticalSeparator(p=HFrame_27, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 如果不是 FXHorizontalFrame 实例
        else:
            # 则插入一个水平分隔符
            FXHorizontalSeparator(p=HFrame_27, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 创建一个垂直框架 VFrame_16
        # p: 父窗口部件 (HFrame_27)
        # opts: 选项 (0表示默认或无特定选项)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        VFrame_16 = FXVerticalFrame(p=HFrame_27, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        # 创建一个水平框架 HFrame_38
        # p: 父窗口部件 (VFrame_16)
        # opts: 选项 (0表示默认或无特定选项)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        HFrame_38 = FXHorizontalFrame(p=VFrame_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        # 获取当前的模型对象
        m = self.get_current_model()
        # 将模型的名称设置到表单的 keyword99Kw 字段中
        form.keyword99Kw.setValue(m.name)
        # 创建一个文本输入框 ModelNameTxt，用于显示模型名称
        # p: 父窗口部件 (HFrame_38)
        # ncols: 列数 (文本框的宽度)
        # labelText: 标签文本，使用GB18030编码
        # tgt: 目标关键字 (form.keyword99Kw)，用于数据绑定
        # sel: 选择器 (0表示默认)
        ModelNameTxt=AFXTextField(p=HFrame_38, ncols=12, labelText=u'模型:'.encode('GB18030'), tgt=form.keyword99Kw, sel=0)
        # 隐藏模型名称文本框
        ModelNameTxt.hide()
        # 创建一个组合框 ComboBox_15，用于选择目标材料
        # p: 父窗口部件 (HFrame_38)
        # ncols: 列数 (0表示自动调整)
        # nvis: 可见项数 (1表示只显示当前选中项)
        # text: 组合框的标签文本，使用GB18030编码
        # tgt: 目标关键字 (form.keyword98Kw)，用于数据绑定
        # sel: 选择器 (self.ID_MODEL_MATERIAL_COMBO_CHANGED)，用于事件处理
        self.ComboBox_15 = AFXComboBox(p=HFrame_38, ncols=0, nvis=1, text=u'目标材料:'.encode('GB18030'), tgt=form.keyword98Kw, sel=self.ID_MODEL_MATERIAL_COMBO_CHANGED)
        # 设置组合框的目标对象为当前实例 (self)
        self.ComboBox_15.setTarget(self)
        # 设置组合框的选择器，当选择改变时触发 ID_MODEL_MATERIAL_COMBO_CHANGED 事件
        self.ComboBox_15.setSelector(self.ID_MODEL_MATERIAL_COMBO_CHANGED)
        # 将 SEL_COMMAND 事件与 ID_MODEL_MATERIAL_COMBO_CHANGED 选择器关联到 STPM_test1033DB.model_meterial_changed 方法
        # 这是事件处理的映射机制
        FXMAPFUNC(self, SEL_COMMAND, self.ID_MODEL_MATERIAL_COMBO_CHANGED, STPM_test1033DB.model_meterial_changed)
        # 设置组合框的最大可见项数为10
        self.ComboBox_15.setMaxVisible(10)
        # 遍历模型中所有材料的键，并将它们添加到组合框中作为可选项
        for material in m.materials.keys():
            self.ComboBox_15.appendItem(text=material)
        # 在组合框中添加一个名为 'New' 的选项
        self.ComboBox_15.appendItem(text='New')
        # 创建一个文本输入框 newMaterialText，用于输入新材料的名称
        # p: 父窗口部件 (HFrame_38)
        # ncols: 列数 (文本框的宽度)
        # labelText: 标签文本 'NewName:'
        # tgt: 目标关键字 (form.keyword100Kw)，用于数据绑定
        # sel: 选择器 (0表示默认)
        # opts: 选项 (0表示默认)
        self.newMaterialText = AFXTextField(p=HFrame_38, ncols=12, labelText='NewName:', 
            tgt=form.keyword100Kw, sel=0, opts=0)
        # 判断当前组合框选中的项是否不是 'New'
        # 如果不是 'New'，则隐藏用于输入新材料名称的文本框
        # 注释 #2025年6月4日 lgp  是代码修改日期和作者
        if self.ComboBox_15.getItemText(self.ComboBox_15.getCurrentItem())!='New': #2025年6月4日 lgp
            self.newMaterialText.hide()
        # 这是一行被注释掉的代码， 是一个备用或旧的文本框定义
        # AFXTextField(p=HFrame_38, ncols=12, labelText='New:', tgt=form.keyword100Kw, sel=0)
        # 创建另一个水平框架 HFrame_31
        # p: 父窗口部件 (VFrame_16)
        # opts: 选项 (0表示默认)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        HFrame_31 = FXHorizontalFrame(p=VFrame_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        # 创建一个垂直对齐器 VAligner_9
        # p: 父窗口部件 (HFrame_31)
        # opts: 选项 (0表示默认)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        VAligner_9 = AFXVerticalAligner(p=HFrame_31, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        # 创建第一个微调器 (spinner1)，用于设置 'UVARM个数'
        # VAligner_9: 父窗口部件
        # 6: 文本框的宽度
        # u'UVARM个数'.encode('GB18030'): 标签文本
        # form.keyword61Kw: 目标关键字，用于数据绑定
        # 0: 默认值
        spinner1 = AFXSpinner(VAligner_9, 6, u'UVARM个数'.encode('GB18030'), form.keyword61Kw, 0)
        # 设置微调器的取值范围为 0 到 100
        spinner1.setRange(0, 100)
        # 设置微调器的增量为 1
        spinner1.setIncrement(1)
        # 创建第二个微调器 (spinner2)，用于设置 'SDV个数'
        # VAligner_9: 父窗口部件
        # 6: 文本框的宽度
        # u'SDV个数'.encode('GB18030'): 标签文本
        # form.keyword62Kw: 目标关键字，用于数据绑定
        # 0: 默认值
        spinner2 = AFXSpinner(VAligner_9, 6, u'SDV个数'.encode('GB18030'), form.keyword62Kw, 0)
        # 设置微调器的取值范围为 0 到 9999
        spinner2.setRange(0, 9999)
        # 设置微调器的增量为 1
        spinner2.setIncrement(1)
        # 创建一个按钮 import1，用于“导入该材料”
        # p: 父窗口部件 (VFrame_16)
        # text: 按钮文本，使用GB18030编码
        # ic: 图标 (None表示无图标)
        # tgt: 目标对象 (self)
        # sel: 选择器 (self.ID_CLICKED_IMPORT1)，用于事件处理
        # opts: 选项 (BUTTON_NORMAL | LAYOUT_CENTER_Y 表示普通按钮并垂直居中)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        import1 = FXButton(p=VFrame_16, text=u'导入该材料'.encode('GB18030'), ic=None, tgt=self,
                                     sel=self.ID_CLICKED_IMPORT1,
                                     opts=BUTTON_NORMAL |LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        # 这是一行被注释掉的代码， 是一个备用或旧的标签定义
        #l = FXLabel(p=VFrame_16, text='Button: Run', opts=JUSTIFY_LEFT)
        # 为 import1 按钮添加事件处理的注释说明
        # 设置 import1 按钮的目标对象为当前实例 (self)
        import1.setTarget(self)
        # 设置 import1 按钮的选择器，当点击时触发 ID_CLICKED_IMPORT1 事件
        import1.setSelector(self.ID_CLICKED_IMPORT1)
        # 将 SEL_COMMAND 事件与 ID_CLICKED_IMPORT1 选择器关联到 STPM_test1033DB.onImport1Clicked 方法
        # 这是事件处理的映射机制
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_IMPORT1, STPM_test1033DB.onImport1Clicked)
        # 创建一个标签页项 tabItem，用于“幅值表”
        # p: 父窗口部件 (TabBook_1)
        # text: 标签页文本，使用GB18030编码
        # ic: 图标 (None表示无图标)
        # opts: 选项 (TAB_TOP_NORMAL 表示顶部普通标签页)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        tabItem = FXTabItem(p=TabBook_1, text=u'幅值表'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        # 创建一个垂直框架 TabItem_6，作为标签页的内容区域
        # p: 父窗口部件 (TabBook_1)
        # opts: 选项 (FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X 表示凸起、粗边框并填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        # hs, vs: 水平间距和垂直间距
        TabItem_6 = FXVerticalFrame(p=TabBook_1,
                                    opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                    x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                    pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # 创建一个分组框 GroupBox_5，标题为“数据格式”
        # p: 父窗口部件 (TabItem_6)
        # text: 分组框标题，使用GB18030编码
        # opts: 选项 (FRAME_GROOVE | LAYOUT_FILL_X 表示凹槽边框并填充水平空间)
        GroupBox_5 = FXGroupBox(p=TabItem_6, text=u'数据格式'.encode('GB18030'), opts=FRAME_GROOVE | LAYOUT_FILL_X)
        # 隐藏数据格式分组框
        GroupBox_5.hide()
        # 创建一个水平框架 HFrame_8
        # p: 父窗口部件 (GroupBox_5)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        HFrame_8 = FXHorizontalFrame(p=GroupBox_5, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                     pl=0, pr=0, pt=0, pb=0)
        # 创建一个分组框 GroupBox_2，标题为“Separated XY*”
        # p: 父窗口部件 (HFrame_8)
        # text: 分组框标题
        # opts: 选项 (FRAME_GROOVE | LAYOUT_FILL_X 表示凹槽边框并填充水平空间)
        GroupBox_2 = FXGroupBox(p=HFrame_8, text='Separated XY*', opts=FRAME_GROOVE | LAYOUT_FILL_X)
        # 这是一行被注释掉的代码， 用于隐藏 GroupBox_2
        # GroupBox_2.hide()
        # 创建一个水平框架 HFrame_6
        # p: 父窗口部件 (GroupBox_2)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        HFrame_6 = FXHorizontalFrame(p=GroupBox_2, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                     pl=0, pr=0, pt=0, pb=0)
        # 创建一个单选按钮，文本为 'Separated'
        # p: 父窗口部件 (HFrame_6)
        # text: 按钮文本
        # tgt: 目标关键字 (form.DataXYTypeKw1)，用于数据绑定
        # sel: 选择器 (53)
        FXRadioButton(p=HFrame_6, text='Separated', tgt=form.DataXYTypeKw1, sel=53)
        # 创建另一个单选按钮，文本为 'Unified X'
        # p: 父窗口部件 (HFrame_6)
        # text: 按钮文本
        # tgt: 目标关键字 (form.DataXYTypeKw1)，用于数据绑定
        # sel: 选择器 (54)
        FXRadioButton(p=HFrame_6, text='Unified X', tgt=form.DataXYTypeKw1, sel=54)
        # 创建一个分组框 GroupBox_3，标题为“Based On...”
        # p: 父窗口部件 (HFrame_8)
        # text: 分组框标题
        # opts: 选项 (FRAME_GROOVE | LAYOUT_FILL_X 表示凹槽边框并填充水平空间)
        GroupBox_3 = FXGroupBox(p=HFrame_8, text='Based On...', opts=FRAME_GROOVE | LAYOUT_FILL_X)
        # 这是一行被注释掉的代码， 用于隐藏 GroupBox_3
        # GroupBox_3.hide()
        # 创建一个水平框架 HFrame_7
        # p: 父窗口部件 (GroupBox_3)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        HFrame_7 = FXHorizontalFrame(p=GroupBox_3, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                     pl=0, pr=0, pt=0, pb=0)
        # 创建一个单选按钮，文本为 'Step Time'
        # p: 父窗口部件 (HFrame_7)
        # text: 按钮文本
        # tgt: 目标关键字 (form.DataTimeTypeKw1)，用于数据绑定
        # sel: 选择器 (55)
        FXRadioButton(p=HFrame_7, text='Step Time', tgt=form.DataTimeTypeKw1, sel=55)
        # 检查 HFrame_7 是否是 FXHorizontalFrame 实例
        # 根据父容器的类型来决定使用垂直分隔符还是水平分隔符
        if isinstance(HFrame_7, FXHorizontalFrame):
            # 如果是水平框架，则插入一个垂直分隔符
            FXVerticalSeparator(p=HFrame_7, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 如果不是 FXHorizontalFrame 实例
        else:
            # 则插入一个水平分隔符
            FXHorizontalSeparator(p=HFrame_7, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 创建一个单选按钮，文本为 'Time(h)' (小时)
        # p: 父窗口部件 (HFrame_7)
        # text: 按钮文本
        # tgt: 目标关键字 (form.TimeUnitKw1)，用于数据绑定
        # sel: 选择器 (56)
        FXRadioButton(p=HFrame_7, text='Time(h)', tgt=form.TimeUnitKw1, sel=56)
        # 创建另一个单选按钮，文本为 'Time(s)' (秒)
        # p: 父窗口部件 (HFrame_7)
        # text: 按钮文本
        # tgt: 目标关键字 (form.TimeUnitKw1)，用于数据绑定
        # sel: 选择器 (57)
        FXRadioButton(p=HFrame_7, text='Time(s)', tgt=form.TimeUnitKw1, sel=57)
        # 创建一个文件处理器 InputFileHandler
        # form: 表单对象
        # self: 当前实例
        # 'InputDataName': 关键字名称
        # '(*.xls)': 文件过滤器，只显示xls文件
        fileHandler = InputFileHandler(form, self, 'InputDataName', '(*.xls)')
        # 创建一个水平框架 fileTextHf，用于文件输入相关的控件
        # p: 父窗口部件 (TabItem_6)
        # opts: 选项 (0表示默认)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        # hs, vs: 水平间距和垂直间距
        fileTextHf = FXHorizontalFrame(p=TabItem_6, opts=0, x=0, y=0, w=0, h=0,
                                       pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # 注释：设置选择器，以指示此窗口部件在“颜色布局管理器”按钮被选中时，不应与父部件颜色不同。
        # 这是RSG对话框构建器中的一个特定设置。
        fileTextHf.setSelector(99)
        # 创建一个文本输入框，用于显示“幅值表xls文件”的路径
        # p: 父窗口部件 (fileTextHf)
        # ncols: 列数 (70)
        # labelText: 标签文本，使用GB18030编码
        # tgt: 目标关键字 (form.InputDataNameKw)，用于数据绑定
        # sel: 选择器 (0表示默认)
        # opts: 选项 (AFXTEXTFIELD_STRING | LAYOUT_CENTER_Y 表示字符串类型文本框并垂直居中)
        AFXTextField(p=fileTextHf, ncols=70, labelText=u'幅值表xls文件:'.encode('GB18030'), tgt=form.InputDataNameKw, sel=0,
                     opts=AFXTEXTFIELD_STRING | LAYOUT_CENTER_Y)
        # 获取一个文件打开图标
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL)
        # 创建一个按钮，用于“从对话框选择文件”
        # p: 父窗口部件 (fileTextHf)
        # text: 按钮文本
        # ic: 图标 (文件打开图标)
        # tgt: 目标对象 (fileHandler)，点击按钮会激活文件处理器
        # sel: 选择器 (AFXMode.ID_ACTIVATE)，激活文件选择对话框
        # opts: 选项 (BUTTON_NORMAL | LAYOUT_CENTER_Y 表示普通按钮并垂直居中)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
                 opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        # 检查 TabItem_6 是否是 FXHorizontalFrame 实例
        # 根据父容器的类型来决定使用垂直分隔符还是水平分隔符
        if isinstance(TabItem_6, FXHorizontalFrame):
            # 如果是水平框架，则插入一个垂直分隔符
            FXVerticalSeparator(p=TabItem_6, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 如果不是 FXHorizontalFrame 实例
        else:
            # 则插入一个水平分隔符
            FXHorizontalSeparator(p=TabItem_6, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 创建一个按钮 run2，用于“导入幅值表”
        # p: 父窗口部件 (TabItem_6)
        # text: 按钮文本，使用GB18030编码
        # ic: 图标 (None表示无图标)
        # tgt: 目标对象 (fileHandler)，点击按钮会激活文件处理器
        # sel: 选择器 (AFXMode.ID_ACTIVATE + 1)， 用于触发文件处理后的特定动作
        # opts: 选项 (BUTTON_NORMAL 表示普通按钮)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        run2 = FXButton(p=TabItem_6, text=u'导入幅值表'.encode('GB18030'), ic=None, tgt=fileHandler,
                        sel=AFXMode.ID_ACTIVATE + 1,
                        opts=BUTTON_NORMAL, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        # 这是一行被注释掉的代码， 是一个备用或旧的标签定义
        # l = FXLabel(p=TabItem_6, text='Button: Run', opts=JUSTIFY_LEFT)
        # 设置 run2 按钮的目标对象为当前实例 (self)
        run2.setTarget(self)
        # 设置 run2 按钮的选择器，当点击时触发 ID_CLICKED_IMPORTFUZHI 事件
        run2.setSelector(self.ID_CLICKED_IMPORTFUZHI)
        # 将 SEL_COMMAND 事件与 ID_CLICKED_IMPORTFUZHI 选择器关联到 STPM_test1033DB.Clicked_amplitude 方法
        # 这是事件处理的映射机制
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_IMPORTFUZHI, STPM_test1033DB.Clicked_amplitude)
        # 设置标签 l 的字体为粗体
        # 注意：这里的 l 变量在前面被注释掉了，如果代码执行到这里，l  是之前某个地方定义的最后一个 FXLabel 实例
        l.setFont(getAFXFont(FONT_BOLD))
        # 创建一个水平框架 HFrame_10
        # p: 父窗口部件 (TabItem_6)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        HFrame_10 = FXHorizontalFrame(p=TabItem_6, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        # 创建一个分组框 GroupBox_7，标题为“*Separated Example”
        # p: 父窗口部件 (HFrame_10)
        # text: 分组框标题
        # opts: 选项 (FRAME_GROOVE 表示凹槽边框)
        GroupBox_7 = FXGroupBox(p=HFrame_10, text='*Separated Example', opts=FRAME_GROOVE)
        # 隐藏分组框 GroupBox_7
        GroupBox_7.hide()
        # 创建一个水平框架 HFrame_9
        # p: 父窗口部件 (GroupBox_7)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        HFrame_9 = FXHorizontalFrame(p=GroupBox_7, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                     pl=0, pr=0, pt=0, pb=0)
        # 创建一个垂直框架 VFrame_1
        # p: 父窗口部件 (HFrame_9)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        VFrame_1 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        # 在 VFrame_1 中创建标签 'Time1'
        l = FXLabel(p=VFrame_1, text='Time1', opts=JUSTIFY_LEFT)
        # 在 VFrame_1 中创建标签 '0'
        l = FXLabel(p=VFrame_1, text='0', opts=JUSTIFY_LEFT)
        # 在 VFrame_1 中创建标签 '1'
        l = FXLabel(p=VFrame_1, text='1', opts=JUSTIFY_LEFT)
        # 创建一个垂直框架 VFrame_2
        # p: 父窗口部件 (HFrame_9)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        VFrame_2 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        # 在 VFrame_2 中创建标签 'Amp1'
        l = FXLabel(p=VFrame_2, text='Amp1', opts=JUSTIFY_LEFT)
        # 在 VFrame_2 中创建标签 '0'
        l = FXLabel(p=VFrame_2, text='0', opts=JUSTIFY_LEFT)
        # 在 VFrame_2 中创建标签 '100'
        l = FXLabel(p=VFrame_2, text='100', opts=JUSTIFY_LEFT)
        # 检查 HFrame_9 是否是 FXHorizontalFrame 实例
        # 根据父容器的类型来决定使用垂直分隔符还是水平分隔符
        if isinstance(HFrame_9, FXHorizontalFrame):
            # 如果是水平框架，则插入一个垂直分隔符
            FXVerticalSeparator(p=HFrame_9, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 如果不是 FXHorizontalFrame 实例
        else:
            # 则插入一个水平分隔符
            FXHorizontalSeparator(p=HFrame_9, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 创建一个垂直框架 VFrame_3
        # p: 父窗口部件 (HFrame_9)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        VFrame_3 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        # 在 VFrame_3 中创建标签 'Time2'
        l = FXLabel(p=VFrame_3, text='Time2', opts=JUSTIFY_LEFT)
        # 在 VFrame_3 中创建标签 '0'
        l = FXLabel(p=VFrame_3, text='0', opts=JUSTIFY_LEFT)
        # 在 VFrame_3 中创建标签 '0.5'
        l = FXLabel(p=VFrame_3, text='0.5', opts=JUSTIFY_LEFT)
        # 在 VFrame_3 中创建标签 '1'
        l = FXLabel(p=VFrame_3, text='1', opts=JUSTIFY_LEFT)
        # 创建一个垂直框架 VFrame_4
        # p: 父窗口部件 (HFrame_9)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w=0, h=0: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        VFrame_4 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        # 在 VFrame_4 中创建标签 'Amp2'
        l = FXLabel(p=VFrame_4, text='Amp2', opts=JUSTIFY_LEFT)
        # 在 VFrame_4 中创建标签 '0'
        l = FXLabel(p=VFrame_4, text='0', opts=JUSTIFY_LEFT)
        # 在 VFrame_4 中创建标签 '100'
        l = FXLabel(p=VFrame_4, text='100', opts=JUSTIFY_LEFT)
        # 在 VFrame_4 中创建标签 '500'
        l = FXLabel(p=VFrame_4, text='500', opts=JUSTIFY_LEFT)
        # 检查 HFrame_10 是否是 FXHorizontalFrame 实例
        # 根据父容器的类型来决定使用垂直分隔符还是水平分隔符
        if isinstance(HFrame_10, FXHorizontalFrame):
            # 如果是水平框架，则插入一个垂直分隔符
            FXVerticalSeparator(p=HFrame_10, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 如果不是 FXHorizontalFrame 实例
        else:
            # 则插入一个水平分隔符
            FXHorizontalSeparator(p=HFrame_10, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 创建一个分组框 GroupBox_8，标题为“*Unified Example”
        # p: 父窗口部件 (HFrame_10)
        # text: 分组框标题
        # opts: 选项 (FRAME_GROOVE 表示凹槽边框)
        GroupBox_8 = FXGroupBox(p=HFrame_10, text='*Unified Example', opts=FRAME_GROOVE)
        # 隐藏分组框 GroupBox_8
        GroupBox_8.hide()
        # 创建一个水平框架 HFrame_11
        # p: 父窗口部件 (GroupBox_8)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        HFrame_11 = FXHorizontalFrame(p=GroupBox_8, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        # 创建一个垂直框架 VFrame_7
        # p: 父窗口部件 (HFrame_11)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        VFrame_7 = FXVerticalFrame(p=HFrame_11, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        # 在 VFrame_7 中创建标签 'Time'
        l = FXLabel(p=VFrame_7, text='Time', opts=JUSTIFY_LEFT)
        # 在 VFrame_7 中创建标签 '0'
        l = FXLabel(p=VFrame_7, text='0', opts=JUSTIFY_LEFT)
        # 在 VFrame_7 中创建标签 '0.5'
        l = FXLabel(p=VFrame_7, text='0.5', opts=JUSTIFY_LEFT)
        # 在 VFrame_7 中创建标签 '1'
        l = FXLabel(p=VFrame_7, text='1', opts=JUSTIFY_LEFT)
        # 检查 HFrame_11 是否是 FXHorizontalFrame 实例
        # 根据父容器的类型来决定使用垂直分隔符还是水平分隔符
        if isinstance(HFrame_11, FXHorizontalFrame):
            # 如果是水平框架，则插入一个垂直分隔符
            FXVerticalSeparator(p=HFrame_11, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 如果不是 FXHorizontalFrame 实例
        else:
            # 则插入一个水平分隔符
            FXHorizontalSeparator(p=HFrame_11, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        # 创建一个垂直框架 VFrame_6
        # p: 父窗口部件 (HFrame_11)
        # opts: 选项 (LAYOUT_FILL_X 表示填充水平空间)
        # x, y, w, h: 位置和尺寸 (通常为0，由布局管理器控制)
        # pl, pr, pt, pb: 内边距 (左、右、上、下)
        VFrame_6 = FXVerticalFrame(p=HFrame_11, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
        # 在 VFrame_6 中创建标签 'Amp1'
        l = FXLabel(p=VFrame_6, text='Amp1', opts=JUSTIFY_LEFT)
        # 在 VFrame_6 中创建标签 '0'
        l = FXLabel(p=VFrame_6, text='0', opts=JUSTIFY_LEFT)
        # 在 VFrame_6 中创建标签 '50'
        l = FXLabel(p=VFrame_6, text='50', opts=JUSTIFY_LEFT)
        # 创建一个标签，显示文本 '100'，并左对齐。
        
        l = FXLabel(p=VFrame_6, text='100', opts=JUSTIFY_LEFT)



        # 创建一个垂直框架 VFrame_5，作为 HFrame_11 的子组件。
        # 该框架将填充其父组件的水平空间 (LAYOUT_FILL_X)。
        VFrame_5 = FXVerticalFrame(p=HFrame_11, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                   pl=0, pr=0, pt=0, pb=0)
    # 在 VFrame_5 中创建一个标签，显示文本 'Amp2'，并左对齐。
        l = FXLabel(p=VFrame_5, text='Amp2', opts=JUSTIFY_LEFT)
    # 在 VFrame_5 中创建一个标签，显示文本 '0'，并左对齐。
        l = FXLabel(p=VFrame_5, text='0', opts=JUSTIFY_LEFT)
    # 在 VFrame_5 中创建一个标签，显示文本 '100'，并左对齐。
        l = FXLabel(p=VFrame_5, text='100', opts=JUSTIFY_LEFT)
    # 在 VFrame_5 中创建一个标签，显示文本 '500'，并左对齐。
        l = FXLabel(p=VFrame_5, text='500', opts=JUSTIFY_LEFT)
# 创建一个标签页项 tabItem，属于 TabBook_1。
# 标签文本为 '分析步'，编码为 GB18030。
# 标签位于顶部，样式为普通。
        tabItem = FXTabItem(p=TabBook_1, text=u'分析步'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
# 设置标签页项的初始位置和大小，以及内边距。
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
# 创建一个垂直框架 TabItem_5，作为 TabBook_1 的子组件。
# 框架具有凸起和粗边框样式，并填充其父组件的水平空间。
        TabItem_5 = FXVerticalFrame(p=TabBook_1,
                                    opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
# 设置框架的初始位置、大小和内边距，以及水平和垂直间距。
                                    x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                    pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
# 创建一个水平框架 HFrame_32，作为 TabItem_5 的子组件。
# 框架选项为0（默认），并设置内边距为0。
        HFrame_32 = FXHorizontalFrame(p=TabItem_5, opts=0, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
# 创建一个垂直框架 listVf，作为 HFrame_32 的子组件。
# 框架具有凹陷和粗边框样式，并设置内边距为0。
        listVf = FXVerticalFrame(p=HFrame_32, opts=FRAME_SUNKEN | FRAME_THICK, x=0, y=0, w=0, h=0,
                                 pl=0, pr=0, pt=0, pb=0)
# 设置 TabBook_1 的目标对象为当前实例 (self)，以便处理其事件。
        TabBook_1.setTarget(self)                 # 监听顶层标签切换
# 设置 TabBook_1 的选择器，当标签页改变时会触发 ID_TAB_CHANGED 事件。
        TabBook_1.setSelector(self.ID_TAB_CHANGED)
# 映射 SEL_COMMAND 类型的 ID_TAB_CHANGED 事件到 STPM_test1033DB.onMainTabChanged 方法。
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TAB_CHANGED,
                STPM_test1033DB.onMainTabChanged)
# Note: Set the selector to indicate that this widget should not be
#       colored differently from its parent when the 'Color layout managers'
#       button is checked in the RSG Dialog Builder dialog.
# 设置 listVf 的选择器为 99。
        listVf.setSelector(99)
# 创建一个 AFXList 列表组件 List_3，作为 listVf 的子组件。
# 可见项数为 10，目标为 form.keyword64Kw，选择模式为单选且不显示水平滚动条。
        self.List_3 = AFXList(p=listVf, nvis=10, tgt=form.keyword64Kw, sel=0, opts=HSCROLLING_OFF | LIST_SINGLESELECT)
# 向 List_3 中添加列表项 '1'。
        self.List_3.appendItem(text='1')
# 向 List_3 中添加列表项 '2'。
        self.List_3.appendItem(text='2')
# 向 List_3 中添加列表项 '3'。
        self.List_3.appendItem(text='3')
# 向 List_3 中添加列表项 '4'。
        self.List_3.appendItem(text='4')
# 向 List_3 中添加列表项 '5'。
        self.List_3.appendItem(text='5')

# 设置 List_3 的目标对象为当前实例 (self)。
        self.List_3.setTarget(self)
# 设置 List_3 的选择器，当列表项被点击时会触发 ID_CLICKED_LIST 事件。
        self.List_3.setSelector(self.ID_CLICKED_LIST)
# 映射 SEL_DOUBLECLICKED 类型的 ID_CLICKED_LIST 事件到 self.onListItemDoubleClicked 方法。
        FXMAPFUNC(self, SEL_DOUBLECLICKED, self.ID_CLICKED_LIST, self.onListItemDoubleClicked)

# 创建一个垂直框架 VFrame_17，作为 HFrame_32 的子组件。
# 框架选项为0（默认），并设置内边距为0。
        VFrame_17 = FXVerticalFrame(p=HFrame_32, opts=0, x=0, y=0, w=0, h=0,
                                    pl=0, pr=0, pt=0, pb=0)
# 创建一个垂直对齐器 VAligner_10，作为 VFrame_17 的子组件。
# 对齐器选项为0（默认），并设置内边距为0。
        VAligner_10 = AFXVerticalAligner(p=VFrame_17, opts=0, x=0, y=0, w=0, h=0,
                                         pl=0, pr=0, pt=0, pb=0)
# 在 VAligner_10 中创建一个文本输入框，用于显示“循环前分析步:”。
# 文本框宽度为 50 列，目标为 form.keyword68Kw。
        AFXTextField(p=VAligner_10, ncols=50, labelText=u'     循环前分析步:'.encode('GB18030'), tgt=form.keyword68Kw, sel=0)
# 在 VAligner_10 中创建一个文本输入框，用于显示“>>循环分析步构成:”。
# 文本框宽度为 50 列，目标为 form.keyword65Kw。
        AFXTextField(p=VAligner_10, ncols=50, labelText=u'>>循环分析步构成:'.encode('GB18030'), tgt=form.keyword65Kw, sel=0)
# 将 form.keyword65Kw 的值设置为空字符串。
        form.keyword65Kw.setValue("")
# 设置 form.keyword65Kw 的目标对象为当前实例 (self)。
        self.form.keyword65Kw.setTarget(self)
# 设置 form.keyword65Kw 的选择器，当文本改变时会触发 ID_CYCLE_LIST_CHANGED 事件。
        self.form.keyword65Kw.setSelector(self.ID_CYCLE_LIST_CHANGED)
# 映射 SEL_COMMAND 类型的 ID_CYCLE_LIST_CHANGED 事件到 self.onCycleListChanged 方法。
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CYCLE_LIST_CHANGED, self.onCycleListChanged)

# 在 VAligner_10 中创建一个文本输入框，用于显示“循环后分析步:”。
# 文本框宽度为 50 列，目标为 form.keyword69Kw。
        AFXTextField(p=VAligner_10, ncols=50, labelText=u'     循环后分析步:'.encode('GB18030'), tgt=form.keyword69Kw, sel=0)
# 在 VAligner_10 中创建一个文本输入框，用于显示“循环次数:”。
# 文本框宽度为 12 列，目标为 form.keyword77Kw。
        AFXTextField(p=VAligner_10, ncols=12, labelText=u'     循环次数:'.encode('GB18030'), tgt=form.keyword77Kw, sel=0)
# 在 VAligner_10 中创建一个文本输入框，用于显示“HOLDING步步长:”。
# 文本框宽度为 12 列，目标为 form.keyword94Kw。
        AFXTextField(p=VAligner_10, ncols=12, labelText=u'     HOLDING步步长:'.encode('GB18030'), tgt=form.keyword94Kw, sel=0)
# 创建一个水平框架 HFrame_33，作为 VFrame_17 的子组件。
# 框架选项为0（默认），并设置左内边距为 20。
        HFrame_33 = FXHorizontalFrame(p=VFrame_17, opts=0, x=0, y=0, w=0, h=0,
                                      pl=20, pr=0, pt=0, pb=0)
# 在 HFrame_33 中创建一个单选按钮，文本为 'NEW'。
# 目标为 form.HFrame33Kw1，选择值为 58。
        FXRadioButton(p=HFrame_33, text='NEW', tgt=form.HFrame33Kw1, sel=58)
# 在 HFrame_33 中创建一个单选按钮，文本为 'REPLACE'。
# 目标为 form.HFrame33Kw1，选择值为 59。
        FXRadioButton(p=HFrame_33, text='REPLACE', tgt=form.HFrame33Kw1, sel=59)
# 在 HFrame_33 中创建一个组合框 ComboBox_13，用于选择“分析步类型:”。
# 文本框宽度为 0，可见项数为 1，目标为 form.keyword90Kw。
        ComboBox_13 = AFXComboBox(p=HFrame_33, ncols=0, nvis=1, text=u'           分析步类型:'.encode('GB18030'), tgt=form.keyword90Kw, sel=0)
# 设置组合框的最大可见项数为 10。
        ComboBox_13.setMaxVisible(10)
# 向组合框中添加选项 'AUTO'。
        ComboBox_13.appendItem(text='AUTO')
# 向组合框中添加选项 'StaticStep'。
        ComboBox_13.appendItem(text='StaticStep')
# 向组合框中添加选项 'ViscoStep'。
        ComboBox_13.appendItem(text='ViscoStep')
# 向组合框中添加选项 'HeatTransferStep'。
        ComboBox_13.appendItem(text='HeatTransferStep')
# 创建一个按钮 createstep，文本为 '创建分析步'。
# 按钮作为 VFrame_17 的子组件，目标为 fileHandler。
        createstep = FXButton(p=VFrame_17, text=u'创建分析步'.encode('GB18030'), ic=None, tgt=fileHandler,
# 选择器为 AFXMode.ID_ACTIVATE + 1。
                              sel=AFXMode.ID_ACTIVATE + 1,
# 按钮样式为普通，并垂直居中，设置内边距为 1。
                              opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
# l = FXLabel(p=VFrame_17, text='Button: creat step', opts=JUSTIFY_LEFT)
# 设置 createstep 按钮的目标对象为当前实例 (self)。
        createstep.setTarget(self)
# 设置 createstep 按钮的选择器，当按钮被点击时会触发 ID_CLICKED_CREATESTEP 事件。
        createstep.setSelector(self.ID_CLICKED_CREATESTEP)
# 映射 SEL_COMMAND 类型的 ID_CLICKED_CREATESTEP 事件到 STPM_test1033DB.Createstep 方法。
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_CREATESTEP, STPM_test1033DB.Createstep)
# 判断 TabItem_5 是否是 FXHorizontalFrame 的实例。
        if isinstance(TabItem_5, FXHorizontalFrame):
# 如果是，则在 TabItem_5 中创建一个垂直分隔符。
            FXVerticalSeparator(p=TabItem_5, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
# 否则（即 TabItem_5 不是 FXHorizontalFrame）。
        else:
# 在 TabItem_5 中创建一个水平分隔符。
            FXHorizontalSeparator(p=TabItem_5, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
# 创建一个分组框 GroupBox_23，文本为 '批量设置工具'。
# 分组框作为 TabItem_5 的子组件，样式为凹槽边框。
        GroupBox_23 = FXGroupBox(p=TabItem_5, text=u'批量设置工具'.encode('GB18030'), opts=FRAME_GROOVE)
# 创建一个水平框架 HFrame_34，作为 GroupBox_23 的子组件。
# 框架选项为0（默认），并设置内边距为0。
        HFrame_34 = FXHorizontalFrame(p=GroupBox_23, opts=0, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
# 在 HFrame_34 中创建一个单选按钮，文本为 '\xa1\xa1' (一个特殊字符或占位符)。
# 目标为 form.GroupBox23Kw1，选择值为 60。
        FXRadioButton(p=HFrame_34, text='\xa1\xa1', tgt=form.GroupBox23Kw1, sel=60)
# 在 HFrame_34 中创建一个文本输入框，用于显示“对名字中含有...的分析步:”。
# 文本框宽度为 12 列，目标为 form.keyword72Kw。
        AFXTextField(p=HFrame_34, ncols=12, labelText=u'对名字中含有...的分析步:'.encode('GB18030'), tgt=form.keyword72Kw, sel=0)
# 在 HFrame_34 中创建一个单选按钮，文本为 '或'。
# 目标为 form.GroupBox23Kw1，选择值为 61。
        FXRadioButton(p=HFrame_34, text=u'或'.encode('GB18030'), tgt=form.GroupBox23Kw1, sel=61)
# 在 HFrame_34 中创建一个文本输入框，用于显示“对于从i开始每n的分析步(n,i):”。
# 文本框宽度为 12 列，目标为 form.keyword73Kw。
        AFXTextField(p=HFrame_34, ncols=12, labelText=u'对于从i开始每n的分析步(n,i):'.encode('GB18030'), tgt=form.keyword73Kw, sel=0)
# 创建一个水平框架 HFrame_35，作为 GroupBox_23 的子组件。
# 框架选项为0（默认），并设置内边距为0。
        HFrame_35 = FXHorizontalFrame(p=GroupBox_23, opts=0, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
# 在 HFrame_35 中创建一个组合框 ComboBox_11，用于选择“方法:”。
# 文本框宽度为 0，可见项数为 1，目标为 form.keyword70Kw。
        ComboBox_11 = AFXComboBox(p=HFrame_35, ncols=0, nvis=1, text=u'方法:'.encode('GB18030'), tgt=form.keyword70Kw, sel=0)
# 设置组合框的最大可见项数为 10。
        ComboBox_11.setMaxVisible(10)
# 向组合框中添加选项 'set Time period'。
        ComboBox_11.appendItem(text='set Time period')
# 向组合框中添加选项 'set Initial increment size'。
        ComboBox_11.appendItem(text='set Initial increment size')
# 向组合框中添加选项 'set Max increment size'。
        ComboBox_11.appendItem(text='set Max increment size')
# 向组合框中添加选项 'set Ramp linearly over step'。
        ComboBox_11.appendItem(text='set Ramp linearly over step')
# 向组合框中添加选项 'set Instantaneous'。
        ComboBox_11.appendItem(text='set Instantaneous')
# 向组合框中添加选项 'change Static to Visco'。
        ComboBox_11.appendItem(text='change Static to Visco')
# 向组合框中添加选项 'change Visco to Static'。
        ComboBox_11.appendItem(text='change Visco to Static')
# 向组合框中添加选项 'enable Restart'。
        ComboBox_11.appendItem(text='enable Restart')
# 在 HFrame_35 中创建一个文本输入框，用于显示“ 值:”。
# 文本框宽度为 12 列，目标为 form.keyword74Kw。
        AFXTextField(p=HFrame_35, ncols=12, labelText=u' 值:'.encode('GB18030'), tgt=form.keyword74Kw, sel=0)
# l = FXLabel(p=GroupBox_23, text='Button: modify step', opts=JUSTIFY_LEFT)
# 创建一个按钮 editstep，文本为 '修改分析步'。
# 按钮作为 GroupBox_23 的子组件，目标为当前实例 (self)。
        editstep = FXButton(p=GroupBox_23, text=u'修改分析步'.encode('GB18030'), ic=None, tgt=self,
# 选择器为 self.ID_CLICKED_MODIFYSTEP。
                      sel=self.ID_CLICKED_MODIFYSTEP,
# 按钮样式为普通，并垂直居中，设置内边距为 1。
                      opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
# 设置 editstep 按钮的目标对象为当前实例 (self)。
        editstep.setTarget(self)
# 设置 editstep 按钮的选择器，当按钮被点击时会触发 ID_CLICKED_MODIFYSTEP 事件。
        editstep.setSelector(self.ID_CLICKED_MODIFYSTEP)
# 映射 SEL_COMMAND 类型的 ID_CLICKED_MODIFYSTEP 事件到 STPM_test1033DB.Modifystep 方法。
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_MODIFYSTEP,STPM_test1033DB.Modifystep)
# 创建一个标签页项 tabItem，属于 TabBook_1。
# 标签文本为 '载荷/换热'，编码为 GB18030。
# 标签位于顶部，样式为普通。
        tabItem = FXTabItem(p=TabBook_1, text=u'载荷/换热'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
# 设置标签页项的初始位置和大小，以及内边距。
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
# 创建一个垂直框架 TabItem_7，作为 TabBook_1 的子组件。
# 框架具有凸起和粗边框样式，并填充其父组件的水平空间。
        TabItem_7 = FXVerticalFrame(p=TabBook_1,
                                    opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
# 设置框架的初始位置、大小和内边距，以及水平和垂直间距。
                                    x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                    pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
# 创建一个分组框 GroupBox_26，文本为 '分析步(%OP%)'。
# 分组框作为 TabItem_7 的子组件，样式为凹槽边框。
        GroupBox_26 = FXGroupBox(p=TabItem_7, text=u'分析步(%OP%)'.encode('GB18030'), opts=FRAME_GROOVE)
# 创建一个垂直对齐器 VAligner_11，作为 GroupBox_26 的子组件。
# 对齐器选项为0（默认），并设置内边距为0。
        VAligner_11 = AFXVerticalAligner(p=GroupBox_26, opts=0, x=0, y=0, w=0, h=0,
                                         pl=0, pr=0, pt=0, pb=0)
# 在 VAligner_11 中创建一个文本输入框，用于显示“循环前分析步:”。
# 文本框宽度为 12 列，目标为 form.keyword92Kw。
        AFXTextField(p=VAligner_11, ncols=12, labelText=u'循环前分析步:'.encode('GB18030'), tgt=form.keyword92Kw, sel=0)
# 在 VAligner_11 中创建一个文本输入框，用于显示“循环分析步构成:”。
# 文本框宽度为 50 列，目标为 form.keyword80Kw。
        AFXTextField(p=VAligner_11, ncols=50, labelText=u'循环分析步构成:'.encode('GB18030'), tgt=form.keyword80Kw, sel=0)

# 设置 form.keyword80Kw 的目标对象为当前实例 (self)。
        self.form.keyword80Kw.setTarget(self)
# 设置 form.keyword80Kw 的选择器，当文本改变时会触发 ID_TEXT_CHANGED 事件。
        self.form.keyword80Kw.setSelector(self.ID_TEXT_CHANGED)
# 映射 SEL_COMMAND 类型的 ID_TEXT_CHANGED 事件到 self.onTextChanged 方法。
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TEXT_CHANGED, self.onTextChanged)

# 在 VAligner_11 中创建一个文本输入框，用于显示“循环后分析步:”。
# 文本框宽度为 12 列，目标为 form.keyword93Kw。
        AFXTextField(p=VAligner_11, ncols=12, labelText=u'循环后分析步:'.encode('GB18030'), tgt=form.keyword93Kw, sel=0)
# 在 VAligner_11 中创建一个文本输入框，用于显示“循环次数:”。
# 文本框宽度为 12 列，目标为 form.keyword81Kw。
        AFXTextField(p=VAligner_11, ncols=12, labelText=u'循环次数:'.encode('GB18030'), tgt=form.keyword81Kw, sel=0)
# 创建一个 AFXTextField 文本输入框 importOdbName，用于显示“温度场结果:”。
# 文本框宽度为 50 列，目标为 form.temperatureFieldKw。
        self.importOdbName=AFXTextField(p=TabItem_7, ncols=50, labelText=u'温度场结果:'.encode('GB18030'), tgt=form.temperatureFieldKw, sel=0)
# 创建一个按钮 pairamp，文本为 '匹配幅值表(当前标签页)'。
# 按钮作为 TabItem_7 的子组件，目标为当前实例 (self)。
        pairamp = FXButton(p=TabItem_7, text=u'匹配幅值表(当前标签页)'.encode('GB18030'), ic=None, tgt=self,
# 选择器为 self.ID_CLICKED_AMPPAIR。
                      sel=self.ID_CLICKED_AMPPAIR,
# 按钮样式为普通，并垂直居中，设置内边距为 1。
                      opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
# 设置 pairamp 按钮的目标对象为当前实例 (self)。
        pairamp.setTarget(self)
# 设置 pairamp 按钮的选择器，当按钮被点击时会触发 ID_CLICKED_AMPPAIR 事件。
        pairamp.setSelector(self.ID_CLICKED_AMPPAIR)
# 映射 SEL_COMMAND 类型的 ID_CLICKED_AMPPAIR 事件到 STPM_test1033DB.onCombineCommand 方法。
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_AMPPAIR, STPM_test1033DB.onCombineCommand)
# 创建一个标签页书签 TabBook_5，作为 TabItem_7 的子组件。
# 目标为 None，初始选择项为 0。
        self.TabBook_5 = FXTabBook(p=TabItem_7, tgt=None, sel=0,
# 标签页书签样式为普通。
                              opts=TABBOOK_NORMAL,
# 设置标签页书签的初始位置、大小和内边距。
                              x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                              pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)
# 尝试执行以下代码块，用于获取模型中的相互作用（interactions）。
        try:
            # currentModelName = getCurrentContext().get('modelName', '')
            # x = m.interactions.keys()
            # mw = getAFXApp().getAFXMainWindow()
            # mw.writeToMessageArea(str(x))
# 初始化 HTCList 为空列表。
            self.HTCList = []
# 遍历模型中所有相互作用的键。
            for i in m.interactions.keys():
                # mw.writeToMessageArea(str(m.interactions[i].__name__))
# 如果相互作用的名称是 'FilmCondition'。
                if m.interactions[i].__name__ == 'FilmCondition':
# 将该相互作用的键添加到 HTCList 中。
                    self.HTCList.append(i)

# 捕获 发生的异常。
        except Exception as e:
# 获取 Abaqus GUI 主窗口实例。
            mw = getAFXApp().getAFXMainWindow()
# 向消息区域写入“no model”信息。
            mw.writeToMessageArea("no model")
# 如果发生异常，将 HTCList 初始化为默认值。
            self.HTCList = ['HTC1', 'HTC2']
# 创建一个标签页项 tabItem，属于 self.TabBook_5。
# 标签文本为 '换热'，编码为 GB18030。
# 标签位于顶部，样式为普通。
        tabItem = FXTabItem(p=self.TabBook_5, text=u'换热'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
# 设置标签页项的初始位置和大小，以及内边距。
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
# 创建一个垂直框架 TabItem_20，作为 self.TabBook_5 的子组件。
# 框架具有凸起和粗边框样式，并填充其父组件的水平空间。
        TabItem_20 = FXVerticalFrame(p=self.TabBook_5,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
# 设置框架的初始位置、大小和内边距，以及水平和垂直间距。
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
# 创建一个垂直框架 vf，作为 TabItem_20 的子组件。
# 框架具有凹陷和粗边框样式，并填充其父组件的水平空间。
        vf = FXVerticalFrame(TabItem_20, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
# 设置框架的初始位置、大小和内边距。
                             0, 0, 0, 0, 0, 0, 0, 0)
# Note: Set the selector to indicate that this widget should not be
#       colored differently from its parent when the 'Color layout managers'
#       button is checked in the RSG Dialog Builder dialog.
# 设置 vf 的选择器为 99。
        vf.setSelector(99)
# 创建一个 AFXTable 表格组件 tableH，作为 vf 的子组件。
# 表格有 6 行 4 列，可见行数 6，可见列数 4。
# 目标为 form.keyword78Kw，选择模式为 0，样式为可编辑并填充水平空间。
        self.tableH = AFXTable(vf, 6, 4, 6, 4, form.keyword78Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
# 设置 tableH 的弹出菜单选项，包括剪切、复制、粘贴、插入行、删除行、清空内容、从文件读取和写入文件。
        self.tableH.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
# 设置 tableH 的表头行数为 1。
        self.tableH.setLeadingRows(1)
# 设置 tableH 的表头列数为 1。
        self.tableH.setLeadingColumns(1)

# 设置self.tableH的第1列宽度为200像素
        self.tableH.setColumnWidth(1, 200)
        # 将self.tableH的第1列设置为文本类型
        self.tableH.setColumnType(1, AFXTable.TEXT)
        # 设置self.tableH的第2列宽度为200像素
        self.tableH.setColumnWidth(2, 200)
        # 将self.tableH的第2列设置为文本类型
        self.tableH.setColumnType(2, AFXTable.TEXT)
        # 设置self.tableH的第3列宽度为200像素
        self.tableH.setColumnWidth(3, 200)
        # 将self.tableH的第3列设置为文本类型
        self.tableH.setColumnType(3, AFXTable.TEXT)
        # 设置self.tableH的行标题，使用GB18030编码，包含膜条件、换热幅值表和温度幅值表
        self.tableH.setLeadingRowLabels(u'膜条件(%NM%)\t换热幅值表\t温度幅值表'.encode('GB18030'))
        # 设置self.tableH的最后一列为可伸缩列，使其在表格大小变化时自动调整宽度
        self.tableH.setStretchableColumn(self.tableH.getNumColumns() - 1)
        # 显示self.tableH的水平网格线
        self.tableH.showHorizontalGrid(True)
        # 显示self.tableH的垂直网格线
        self.tableH.showVerticalGrid(True)

        # 检查self.HTCList是否存在且非空
        if self.HTCList:
            # 尝试填充表格数据，以防发生错误
            try:
                # 遍历self.HTCList中的每个元素，从索引0开始
                for i in range(0, len(self.HTCList)):
                    # 设置self.tableH的第i+1行第1列的文本内容为HTCList的当前元素
                    self.tableH.setItemText(i + 1, 1, str(self.HTCList[i]))
                    # 设置self.tableH的第i+1行第2列的文本内容为预定义的字符串
                    self.tableH.setItemText(i + 1, 2, '%OP%_%NM%_HTC')
                    # 设置self.tableH的第i+1行第3列的文本内容为预定义的字符串
                    self.tableH.setItemText(i + 1, 3, '%OP%_%NM%_TEMP')
            # 捕获填充表格时 发生的任何异常
            except Exception as e:
                # 打印错误信息到控制台
                print("Error filling table:", str(e))

        # 尝试获取当前模型并处理其载荷数据
        try:
            # 调用get_current_model方法获取当前的模型对象
            m = self.get_current_model()
            # 初始化self.STRESSList为空列表，用于存储载荷信息
            self.STRESSList = []
            # 获取模型中所有载荷的键（名称）
            model_loads = m.loads.keys() 
            # 检查model_loads列表是否为空
            if not model_loads:
                # 调试信息，如果载荷列表为空，则不执行任何操作（pass）
                # mw.writeToMessageArea(u'Debug: 载荷列表为空'.encode('GB18030'))
                pass
            # 如果载荷列表不为空
            else:
                # 遍历model_loads中的每个载荷名称
                for i in model_loads:
                    # 将载荷名称添加到self.STRESSList中
                    self.STRESSList.append(i)
            # 调试信息，显示STRESSList的内容（被注释掉）
            # mw.writeToMessageArea("Debug: self.STRESSList after try block: " + str(self.STRESSList))
            pass

        # 捕获在try块中发生的任何异常
        except Exception as e:
            # 获取AFX应用程序的主窗口实例
            mw = getAFXApp().getAFXMainWindow()
            # 将捕获到的错误信息写入主窗口的消息区域
            mw.writeToMessageArea(str(e))

        # 创建一个FXTabItem（选项卡项），作为self.TabBook_5的子项
        tabItem = FXTabItem(p=self.TabBook_5, text=u'载荷'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        # 创建一个FXVerticalFrame（垂直框架），作为self.TabBook_5的子项，用于布局
        TabItem_21 = FXVerticalFrame(p=self.TabBook_5,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # 在TabItem_21内部创建一个垂直框架，用于组织内容
        vf = FXVerticalFrame(TabItem_21, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # 这是一个注释，说明设置选择器是为了避免在RSG Dialog Builder中勾选“Color layout managers”时，此部件的颜色与父部件不同。
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        # 设置垂直框架的选择器ID为99
        vf.setSelector(99)
        # 创建一个AFXTable实例，用于显示载荷数据，并设置为可编辑且填充X方向
        self.tableL = AFXTable(vf, 6, 3, 6, 3, form.keyword82Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        # 设置self.tableL的弹出菜单选项，包括剪切、复制、粘贴、插入/删除行、清空内容、从文件读取和写入文件
        self.tableL.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        # 设置self.tableL的固定行数为1（通常用于标题行）
        self.tableL.setLeadingRows(1)
        # 设置self.tableL的固定列数为1（通常用于行标签）
        self.tableL.setLeadingColumns(1)
        # 设置self.tableL的第1列宽度为200像素
        self.tableL.setColumnWidth(1, 200)
        # 将self.tableL的第1列设置为浮点数类型
        self.tableL.setColumnType(1, AFXTable.FLOAT)
        # 设置self.tableL的第2列宽度为100像素
        self.tableL.setColumnWidth(2, 100)
        # 将self.tableL的第2列设置为浮点数类型
        self.tableL.setColumnType(2, AFXTable.FLOAT)
        # 设置self.tableL的行标题，使用GB18030编码，包含载荷和幅值表
        self.tableL.setLeadingRowLabels(u'载荷(%NM%)\t幅值表'.encode('GB18030'))
        # 设置self.tableL的最后一列为可伸缩列
        self.tableL.setStretchableColumn(self.tableL.getNumColumns() - 1)
        # 显示self.tableL的水平网格线
        self.tableL.showHorizontalGrid(True)
        # 显示self.tableL的垂直网格线
        self.tableL.showVerticalGrid(True)
        # 如果self.STRESSList不为空，则填充self.tableL
        if self.STRESSList:
            # 尝试填充表格数据
            try:
                # 遍历self.STRESSList中的每个元素
                for i in range(len(self.STRESSList)):
                    # 如果当前索引i超出了表格的现有行数（减去固定行），则插入新行
                    if i >= self.tableL.getNumRows() - 1: # 如果需要更多行
                        # 在表格末尾插入一行
                        self.tableL.insertRows(self.tableL.getNumRows(), 1) # 在末尾插入新行
                    # 设置self.tableL的第i+1行第1列的文本内容为STRESSList的当前元素
                    self.tableL.setItemText(i + 1, 1, str(self.STRESSList[i]))
                    # 设置self.tableL的第i+1行第2列的文本内容为预定义的字符串
                    self.tableL.setItemText(i + 1, 2, '%OP%_%NM%')
            # 捕获填充表格时 发生的任何异常
            except Exception as e:
                # 打印错误信息到控制台
                print("Error filling table:", str(e))

        # 在TabItem_21内部再次创建一个垂直框架，用于组织其他表格内容
        vf = FXVerticalFrame(TabItem_21, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # 这是一个注释，说明设置选择器是为了避免在RSG Dialog Builder中勾选“Color layout managers”时，此部件的颜色与父部件不同。
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        # 设置垂直框架的选择器ID为99
        vf.setSelector(99)
        # 创建另一个AFXTable实例，用于显示预定义温度场等数据，并设置为可编辑且填充X方向
        self.table1 = AFXTable(vf, 12, 6, 12, 6, form.keyword83Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        # 设置self.table1的固定行数为1
        self.table1.setLeadingRows(1)
        # 设置self.table1的固定列数为1
        self.table1.setLeadingColumns(1)
        # 设置self.table1的第1列宽度为200像素
        self.table1.setColumnWidth(1, 200)
        # 将self.table1的第1列设置为浮点数类型
        self.table1.setColumnType(1, AFXTable.FLOAT)
        # 设置self.table1的第2列宽度为100像素
        self.table1.setColumnWidth(2, 100)
        # 将self.table1的第2列设置为浮点数类型
        self.table1.setColumnType(2, AFXTable.FLOAT)
        # 设置self.table1的第3列宽度为100像素
        self.table1.setColumnWidth(3, 100)
        # 将self.table1的第3列设置为浮点数类型
        self.table1.setColumnType(3, AFXTable.FLOAT)
        # 设置self.table1的第4列宽度为100像素
        self.table1.setColumnWidth(4, 100)
        # 将self.table1的第4列设置为浮点数类型
        self.table1.setColumnType(4, AFXTable.FLOAT)
        # 设置self.table1的第5列宽度为100像素
        self.table1.setColumnWidth(5, 100)
        # 将self.table1的第5列设置为浮点数类型
        self.table1.setColumnType(5, AFXTable.FLOAT)
        # 设置self.table1的行标题，使用GB18030编码，包含多个字段
        self.table1.setLeadingRowLabels(u'预定义温度场（工况）\t开始分析步\t开始增量步\t结束分析步\t结束增量步\t步长\t最大增量步'.encode('GB18030'))
        # 设置self.table1的最后一列为可伸缩列
        self.table1.setStretchableColumn(self.table1.getNumColumns() - 1)
        # 显示self.table1的水平网格线
        self.table1.showHorizontalGrid(True)
        # 显示self.table1的垂直网格线
        self.table1.showVerticalGrid(True)
        # 以下是子程序页面的UI元素定义
        ###子程序页面
        # 创建一个FXTabItem（选项卡项），作为TabBook_1的子项，文本为“子程序”
        tabItem = FXTabItem(p=TabBook_1, text=u'子程序'.encode('GB18030'), ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        # 隐藏这个选项卡项，使其在界面上不可见
        tabItem.hide()
        # 创建一个FXVerticalFrame（垂直框架），作为TabBook_1的子项，用于子程序页面的布局
        TabItem_9 = FXVerticalFrame(p=TabBook_1,
                                    opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                    x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                    pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # 创建一个文件处理器实例，用于处理子程序文件的选择，文件类型限定为.for
        fileHandler = STPM_test1033DBFileHandler(form, 'SubroutineName', '*.for')
        # 创建一个FXHorizontalFrame（水平框架），作为TabItem_9的子项，用于文件选择相关的UI元素布局
        fileTextHf = FXHorizontalFrame(p=TabItem_9, opts=0, x=0, y=0, w=0, h=0,
                                       pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # 这是一个注释，说明设置选择器是为了避免在RSG Dialog Builder中勾选“Color layout managers”时，此部件的颜色与父部件不同。
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        # 设置水平框架的选择器ID为99
        fileTextHf.setSelector(99)
        # 创建一个AFXTextField（文本输入框），用于显示或输入子程序名称
        AFXTextField(p=fileTextHf, ncols=70, labelText='SubroutineName:', tgt=form.SubroutineNameKw, sel=0,
                     opts=AFXTEXTFIELD_STRING | LAYOUT_CENTER_Y)
        # 获取一个文件打开图标，用于按钮显示
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL)
        # 创建一个FXButton（按钮），用于触发文件选择对话框
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
                 opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        # 创建一个FXGroupBox（分组框），标题为“Redefine Creep”，用于组织蠕变相关的设置
        GroupBox_14 = FXGroupBox(p=TabItem_9, text='Redefine Creep', opts=FRAME_GROOVE | LAYOUT_FILL_X)
        # 创建一个AFXComboBox（组合框/下拉菜单），用于选择蠕变本构模型
        ComboBox_9 = AFXComboBox(p=GroupBox_14, ncols=0, nvis=1, text='Creep Consti:', tgt=form.keyword43Kw, sel=0)
        # 设置组合框的最大可见项数为10
        ComboBox_9.setMaxVisible(10)
        # 向组合框添加一个选项“RCC”
        ComboBox_9.appendItem(text='RCC')
        # 向组合框添加一个选项“NB”
        ComboBox_9.appendItem(text='NB')
        # 在GroupBox_14内部创建一个垂直框架，用于组织蠕变参数表格
        vf = FXVerticalFrame(GroupBox_14, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # 这是一个注释，说明设置选择器是为了避免在RSG Dialog Builder中勾选“Color layout managers”时，此部件的颜色与父部件不同。
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        # 设置垂直框架的选择器ID为99
        vf.setSelector(99)
        # 创建一个AFXTable实例，用于显示蠕变参数，并设置为可编辑且填充X方向
        table = AFXTable(vf, 6, 8, 6, 8, form.keyword16Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        # 设置表格的弹出菜单选项，包括剪切、复制、粘贴、插入/删除行、清空内容、从文件读取和写入文件
        table.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        # 设置表格的固定行数为1
        table.setLeadingRows(1)
        # 设置表格的固定列数为1
        table.setLeadingColumns(1)
        # 设置表格的第1列宽度为100像素
        table.setColumnWidth(1, 100)
        # 将表格的第1列设置为浮点数类型
        table.setColumnType(1, AFXTable.FLOAT)
        # 设置表格的第2列宽度为100像素
        table.setColumnWidth(2, 100)
        # 将表格的第2列设置为浮点数类型
        table.setColumnType(2, AFXTable.FLOAT)
        # 设置表格的第3列宽度为100像素
        table.setColumnWidth(3, 100)
        # 将表格的第3列设置为浮点数类型
        table.setColumnType(3, AFXTable.FLOAT)
        # 设置表格的第4列宽度为100像素
        table.setColumnWidth(4, 100)
        # 将表格的第4列设置为浮点数类型
        table.setColumnType(4, AFXTable.FLOAT)
        # 设置表格的第5列宽度为100像素
        table.setColumnWidth(5, 100)
        # 将表格的第5列设置为浮点数类型
        table.setColumnType(5, AFXTable.FLOAT)
        # 设置表格的第6列宽度为100像素
        table.setColumnWidth(6, 100)
        # 将表格的第6列设置为浮点数类型
        table.setColumnType(6, AFXTable.FLOAT)
        # 设置表格的第7列宽度为100像素
        table.setColumnWidth(7, 100)
        # 将表格的第7列设置为浮点数类型
        table.setColumnType(7, AFXTable.FLOAT)
        # 设置表格的行标题，包含温度和p1到p6参数
        table.setLeadingRowLabels('Temp\tp1\tp2\tp3\tp4\tp5\tp6')
        # 设置表格的最后一列为可伸缩列
        table.setStretchableColumn(table.getNumColumns() - 1)
        # 显示表格的水平网格线
        table.showHorizontalGrid(True)
        # 显示表格的垂直网格线
        table.showVerticalGrid(True)
        # 创建一个FXHorizontalFrame（水平框架），作为TabItem_9的子项，用于组织Sr和SN分组框
        HFrame_25 = FXHorizontalFrame(p=TabItem_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
                                      pl=0, pr=0, pt=0, pb=0)
        # 创建一个FXGroupBox（分组框），标题为“Sr”，用于组织Sr相关的设置
        GroupBox_12 = FXGroupBox(p=HFrame_25, text='Sr', opts=FRAME_GROOVE | LAYOUT_FILL_Y)
        # 创建一个FXTabBook（选项卡书签控件），作为GroupBox_12的子项
        TabBook_3 = FXTabBook(p=GroupBox_12, tgt=None, sel=0,
                              opts=TABBOOK_NORMAL,
                              x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                              pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)
        # 创建一个FXTabItem（选项卡项），作为TabBook_3的子项，文本为“Name”
        tabItem = FXTabItem(p=TabBook_3, text='Name', ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        # 创建一个FXVerticalFrame（垂直框架），作为TabBook_3的子项，用于布局
        TabItem_14 = FXVerticalFrame(p=TabBook_3,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # 在TabItem_14内部创建一个垂直框架，用于组织表格内容
        vf = FXVerticalFrame(TabItem_14, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # 这是一个注释，说明设置选择器是为了避免在RSG Dialog Builder中勾选“Color layout managers”时，此部件的颜色与父部件不同。
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        # 设置垂直框架的选择器ID为99
        vf.setSelector(99)
        # 创建一个AFXTable实例，用于显示Sr相关参数，并设置为可编辑且填充X方向
        table = AFXTable(vf, 2, 5, 2, 5, form.keyword35Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        # 设置表格的弹出菜单选项，包括剪切、复制、粘贴、插入/删除行、清空内容、从文件读取和写入文件
        table.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        # 设置表格的固定行数为1
        table.setLeadingRows(1)
        # 设置表格的固定列数为1
        table.setLeadingColumns(1)
        # 设置表格的第1列宽度为50像素
        table.setColumnWidth(1, 50)
        # 将表格的第1列设置为浮点数类型
        table.setColumnType(1, AFXTable.FLOAT)
        # 设置表格的第2列宽度为50像素
        table.setColumnWidth(2, 50)
        # 将表格的第2列设置为浮点数类型
        table.setColumnType(2, AFXTable.FLOAT)
        # 设置表格的第3列宽度为50像素
        table.setColumnWidth(3, 50)
        # 将表格的第3列设置为浮点数类型
        table.setColumnType(3, AFXTable.FLOAT)
        # 设置表格的第4列宽度为50像素
        table.setColumnWidth(4, 50)
        # 将表格的第4列设置为浮点数类型
        table.setColumnType(4, AFXTable.FLOAT)
        # 设置表格的行标题，包含a、b、c、C参数
        table.setLeadingRowLabels('a\tb\tc\tC')
        # 设置表格的最后一列为可伸缩列
        table.setStretchableColumn(table.getNumColumns() - 1)
        # 显示表格的水平网格线
        table.showHorizontalGrid(True)
        # 显示表格的垂直网格线
        table.showVerticalGrid(True)
        # 创建一个FXGroupBox（分组框），标题为“SN”，用于组织SN相关的设置
        GroupBox_22 = FXGroupBox(p=HFrame_25, text='SN', opts=FRAME_GROOVE | LAYOUT_FILL_X | LAYOUT_FILL_Y)
        # 创建一个FXTabBook（选项卡书签控件），作为GroupBox_22的子项
        TabBook_4 = FXTabBook(p=GroupBox_22, tgt=None, sel=0,
                              opts=TABBOOK_NORMAL,
                              x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                              pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)
        # 创建一个FXTabItem（选项卡项），作为TabBook_4的子项，文本为“Name”
        tabItem = FXTabItem(p=TabBook_4, text='Name', ic=None, opts=TAB_TOP_NORMAL,
                            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        # 创建一个FXVerticalFrame（垂直框架），作为TabBook_4的子项，用于布局
        TabItem_18 = FXVerticalFrame(p=TabBook_4,
                                     opts=FRAME_RAISED | FRAME_THICK | LAYOUT_FILL_X,
                                     x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
                                     pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # 在TabItem_18内部创建一个垂直框架，用于组织表格内容
        vf = FXVerticalFrame(TabItem_18, FRAME_SUNKEN | FRAME_THICK | LAYOUT_FILL_X,
                             0, 0, 0, 0, 0, 0, 0, 0)
        # 这是一个注释，说明设置选择器是为了避免在RSG Dialog Builder中勾选“Color layout managers”时，此部件的颜色与父部件不同。
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        # 设置垂直框架的选择器ID为99
        vf.setSelector(99)
        # 创建一个AFXTable实例，用于显示SN相关参数
        table = AFXTable(vf, 6, 5, 6, 5, form.keyword41Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)

# 创建一个表格控件，并将其添加到TabItem_9中
        # 表格的父容器是TabItem_9
        # 表格的名称是'Life'
        # 表格的数据源绑定到form.keyword41Kw
        # 初始行数设置为0
        # 选项包括可编辑和填充X方向
        # 设置表格的右键弹出菜单选项
        # 包含剪切、复制、粘贴、插入行、删除行、清除内容、从文件读取、写入文件等功能
        table.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        # 设置表格的引导行（leading rows）数量为1
        # 引导行通常用于显示行标题或特殊信息
        table.setLeadingRows(1)
        # 设置表格的引导列（leading columns）数量为1
        # 引导列通常用于显示列标题或特殊信息
        table.setLeadingColumns(1)
        # 设置表格第一列的宽度为50像素
        table.setColumnWidth(1, 50)
        # 设置表格第一列的数据类型为浮点型
        table.setColumnType(1, AFXTable.FLOAT)
        # 设置表格第二列的宽度为50像素
        table.setColumnWidth(2, 50)
        # 设置表格第二列的数据类型为浮点型
        table.setColumnType(2, AFXTable.FLOAT)
        # 设置表格第三列的宽度为50像素
        table.setColumnWidth(3, 50)
        # 设置表格第三列的数据类型为浮点型
        table.setColumnType(3, AFXTable.FLOAT)
        # 设置表格第四列的宽度为50像素
        table.setColumnWidth(4, 50)
        # 设置表格第四列的数据类型为浮点型
        table.setColumnType(4, AFXTable.FLOAT)
        # 设置引导行的标签，使用制表符分隔，表示不同的列标题
        table.setLeadingRowLabels('Life\tcol1\tcol2\tcol3')
        # 设置表格的最后一列为可伸缩列，使其自动调整宽度以填充剩余空间
        table.setStretchableColumn(table.getNumColumns() - 1)
        # 显示表格的水平网格线
        table.showHorizontalGrid(True)
        # 显示表格的垂直网格线
        table.showVerticalGrid(True)
        # 创建一个分组框（GroupBox），并将其添加到TabItem_9中
        # 分组框的文本是'Redefine KeyType'
        # 选项包括凹槽边框和填充X方向
        GroupBox_13 = FXGroupBox(p=TabItem_9, text='Redefine KeyType', opts=FRAME_GROOVE | LAYOUT_FILL_X)
        # 创建一个水平框架（HorizontalFrame），并将其添加到GroupBox_13中
        # 框架用于组织内部控件的布局
        HFrame_12 = FXHorizontalFrame(p=GroupBox_13, opts=0, x=0, y=0, w=0, h=0,
                                      # 设置框架的内边距，pl=左，pr=右，pt=上，pb=下
                                      pl=0, pr=0, pt=0, pb=0)
        # 创建一个微调器（Spinner）控件，并将其添加到HFrame_12中
        # 微调器有6个字符宽，标签是'SkipSteps:'
        # 数据源绑定到form.keyword08Kw
        spinner = AFXSpinner(HFrame_12, 6, 'SkipSteps:', form.keyword08Kw, 0)
        # 设置微调器的取值范围，从1到999
        spinner.setRange(1, 999)
        # 设置微调器的增量步长为1
        spinner.setIncrement(1)
        # 创建另一个微调器控件，并将其添加到HFrame_12中
        # 标签是'Every n Step is a Cycle'
        # 数据源绑定到form.keyword07Kw
        spinner = AFXSpinner(HFrame_12, 6, 'Every n Step is a Cycle', form.keyword07Kw, 0)
        # 设置第二个微调器的取值范围，从1到999
        spinner.setRange(1, 999)
        # 设置第二个微调器的增量步长为1
        spinner.setIncrement(1)
        # 创建一个文本输入框（TextField），并将其添加到HFrame_12中
        # 输入框有12列宽，标签是'DownStepLength:'
        # 数据源绑定到form.keyword30Kw
        AFXTextField(p=HFrame_12, ncols=12, labelText='DownStepLength:', tgt=form.keyword30Kw, sel=0)

        # 在TabBook_5创建后添加事件监听
        # 设置TabBook_5的目标对象为当前实例（self），以便接收事件
        self.TabBook_5.setTarget(self)
        # 设置TabBook_5的选择器，当选项卡改变时触发ID_TAB5_CHANGED事件
        self.TabBook_5.setSelector(self.ID_TAB5_CHANGED)
        # 将ID_TAB5_CHANGED事件映射到STPM_test1033DB类的onTabChanged方法
        # SEL_COMMAND表示这是一个命令选择器
        FXMAPFUNC(self, SEL_COMMAND, self.ID_TAB5_CHANGED, STPM_test1033DB.onTabChanged)

    # 主界面DB初始化结束，开始定义函数
    # 定义获取当前模型的方法
    def get_current_model(self):
        """获取当前模型并更新界面显示
        
        # 返回值：
        # Model: 当前模型对象，如果没有则返回 None
        """
        # 从当前上下文获取模型名称，如果不存在则默认为空字符串
        currentModelName = getCurrentContext().get('modelName', '')
        # 检查当前模型名称是否存在于mdb（Abaqus模型数据库）的模型列表中
        if currentModelName in mdb.models:
            # 如果存在，则获取对应的模型对象
            model = mdb.models[currentModelName]
            # 更新界面上ModelName显示，将其值设置为当前模型名称
            self.form.keyword99Kw.setValue(currentModelName)
            # 返回获取到的模型对象
            return model
        # 如果当前模型名称不在模型列表中，则返回None
        return None
    # 定义模型材料改变时的处理函数
    def model_meterial_changed(self,form,sel, ptr):
        # 获取ComboBox_15当前选中项的文本，即选中的材料名称
        selected_material = self.ComboBox_15.getItemText(self.ComboBox_15.getCurrentItem())
        # 根据选择的材料名称显示/隐藏输入框
        if selected_material == "New":
            # 如果选择的是"New"，则显示用于输入新材料名称的文本框
            self.newMaterialText.show()
            # 重新计算并布局newMaterialText的父容器，确保界面正确更新
            self.newMaterialText.getParent().recalc()  # 添加父容器重新布局
        else:
            # 如果选择的不是"New"，则隐藏新材料名称文本框
            self.newMaterialText.hide()
            # 重新计算并布局newMaterialText的父容器，确保界面正确更新
            self.newMaterialText.getParent().recalc()  # 添加父容器重新布局
    #1034版本代码
    # def model_meterial_changed(self, form, sel, ptr):
    #     selected_material = self.ComboBox_15.getItemText(self.ComboBox_15.getCurrentItem())
    #     if selected_material == "New":
    #         self.New_model_meterial_DetailDialog()

    # def New_model_meterial_DetailDialog(self):
    #     detail_dialog = AFXDialog(self, "ADD NEW ", opts=DIALOG_NORMAL, w=0,
    #                               h=0)
    #     vframe = FXVerticalFrame(detail_dialog, opts=LAYOUT_FILL_X | LAYOUT_FILL_Y)
    #     AFXTextField(p=vframe, ncols=12, labelText='New:', sel=0)
    #     addnew = FXButton(p=vframe, text='add', ic=None, tgt=self,
    #                       sel=self.ID_CLICKED_NEW,
    #                       opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
    #     detail_dialog.create()
    #     detail_dialog.show()

    # 定义更新树形视图的方法
    def updateTree(self, material):
        # 清空树形视图中的所有项目
        self.tree.clearItems()
        # 从materials_data字典中获取指定材料的数据，如果不存在则返回空字典
        material_data = self.materials_data.get(material, {})

        # 定义一个内部辅助函数，用于递归地添加子树节点
        def add_subtree(parent, node_name, node_data):
            # 将节点名称转换为字符串类型
            node_name = str(node_name)
            # 如果节点数据是字典类型，表示它是一个父节点
            if isinstance(node_data, dict):
                # 在父节点下添加一个新的子节点，并返回新子节点的引用
                sub_parent = self.tree.addItemLast(parent, node_name)
                # 遍历子节点数据，递归调用add_subtree添加其子项
                for key, value in node_data.items():
                    add_subtree(sub_parent, key, value)
            # 如果节点数据不是字典类型，表示它是一个叶子节点
            else:
                # 直接在父节点下添加该叶子节点
                self.tree.addItemLast(parent, node_name)

            # 展开当前父节点下的所有子树，使其可见
            self.tree.expandTree(parent)

        # 在树形视图的根部添加一个项目，其文本为传入的材料名称
        root = self.tree.addItemLast(None, material)
        # 遍历材料数据中的所有键值对
        for key, value in material_data.items():
            # 为每个顶级键值对调用add_subtree，从根节点开始构建树
            add_subtree(root, key, value)
        # mw = getAFXApp().getAFXMainWindow()
        # mw.writeToMessageArea(str(material_data))
        # 更新树形视图的显示，使其反映最新的数据
        self.tree.update()

            
    # 新增复选框状态变化处理函数
    # 当树形视图中的复选框状态改变时调用此函数
    def onTreeCheckChanged(self, sender, sel, ptr):
        # 获取当前被点击或操作的树形视图项目
        item = self.tree.getCurrentItem()
        # 添加显式状态变量和参数校验
        # 检查项目是否存在且其复选框状态为选中（1表示选中）
        if item and self.tree.getItemCheck(item) == 1:  
            # 获取ComboBox_12当前选中项的文本
            selected_material = self.ComboBox_12.getItemText(self.ComboBox_12.getCurrentItem())
            # mw = getAFXApp().getAFXMainWindow()
            # mw.writeToMessageArea(selected_material)
            # mw.writeToMessageArea(self.tree.getItemText(item))

            # 检查当前选中项的文本是否与ComboBox_12中选中的材料名称相同
            if str(self.tree.getItemText(item)) == selected_material:
                # 如果是根节点被选中
                # 定义一个内部函数，用于选择第一个叶子节点并取消其他叶子节点的选中状态
                def select_first_leaf(node):
                    # 如果当前节点没有子节点（即是叶子节点），则返回True
                    if not node.getFirst():
                        return True
                    
                    # 获取当前节点的第一个子节点
                    child = node.getFirst()
                    # 标志，用于确保只选中第一个叶子节点
                    first_leaf_found = False
                    
                    # 遍历所有子节点
                    while child:
                        # 递归调用select_first_leaf处理当前子节点
                        if select_first_leaf(child):
                            # 如果是第一个找到的叶子节点
                            if not first_leaf_found:
                                # 选中该叶子节点，不触发通知
                                self.tree.setItemCheck(child, True, notify=False)
                                first_leaf_found = True
                            else:
                                # 如果不是第一个叶子节点，则取消选中，不触发通知
                                self.tree.setItemCheck(child, False, notify=False)
                        # 移动到下一个兄弟节点
                        child = child.getNext()
                    
                    # 如果当前节点不是叶子节点，返回False
                    return False

                # 从当前被选中的根节点开始递归处理
                select_first_leaf(item)
                
                # 更新树的显示，使其反映选中状态的变化
                self.tree.update()
            else:
                # 如果被选中的不是根节点，或者根节点与ComboBox_12中的材料不匹配
                # 检查当前项目是否是叶子节点
                if self.tree.isItemLeaf(item):
                    # 获取当前项目的父节点
                    parent = item.getParent()
                    # 如果存在父节点
                    if parent:
                        # 获取父节点的第一个子节点（即当前项目的兄弟节点）
                        sibling = parent.getFirst()
                        # 遍历所有兄弟节点
                        while sibling:
                            # 如果兄弟节点不是当前项目且是叶子节点
                            if sibling != item and self.tree.isItemLeaf(sibling):
                                # 取消选中该兄弟节点，不触发通知
                                self.tree.setItemCheck(sibling, False, notify=False)  #
                            # 移动到下一个兄弟节点
                            sibling = sibling.getNext()
                    # 选中当前叶子节点，不触发通知
                    self.tree.setItemCheck(item, True, notify=False)
                    # 更新树的显示
                    self.tree.update()
                else:
                    # 如果当前项目不是叶子节点（即是父节点）
                    # 循环直到找到一个叶子节点
                    while not self.tree.isItemLeaf(item):
                        # 获取当前节点的第一个子节点
                        child = item.getFirst()
                        # 选中第一个子节点，不触发通知
                        self.tree.setItemCheck(child, True, notify=False)
                        # 获取第一个子节点的下一个兄弟节点
                        sibling = child.getNext()
                        # 遍历所有后续兄弟节点
                        while sibling:
                            # 取消选中这些兄弟节点，不触发通知
                            self.tree.setItemCheck(sibling, False, notify=False)  #
                            # 移动到下一个兄弟节点
                            sibling = sibling.getNext()
                        # 将当前项目设置为其第一个子节点，继续向下遍历
                        item=child
                    # 最终选中找到的叶子节点，不触发通知
                    self.tree.setItemCheck(item, True, notify=False)
                    # 更新树的显示
                    self.tree.update()
                    
    # 定义获取所有被勾选的叶子节点数据的方法
    def get_checked_data(self):
        """获取所有被勾选的叶子节点数据"""
        # 初始化一个空字典用于存储结果数据
        jsondata = {}
        # 获取完整的材料数据
        full_data = self.materials_data
        # 获取Abaqus主窗口实例，用于消息输出
        mw = getAFXApp().getAFXMainWindow()
        
        # 递归获取所有被勾选的节点路径
        def collect_checked_nodes(node, path=None):
            # 如果路径为空，则初始化为空列表
            if path is None:
                path = []
            
            # 如果节点不存在，则返回空列表
            if not node:
                return []
                
            # 初始化结果列表
            result = []
            # 获取当前节点的文本
            node_text = self.tree.getItemText(node)
            # 检查当前节点是否被勾选
            is_checked = self.tree.getItemCheck(node) == 1
            # 构建当前节点的完整路径
            current_path = path + [node_text]
            
            # 如果是叶子节点且被勾选
            if not node.getFirst() and is_checked:
                # 将当前路径添加到结果中
                result.append(current_path)
            
            # 处理子节点
            child = node.getFirst()
            # 遍历所有子节点
            while child:
                # 递归收集子节点中被勾选的路径，并添加到结果中
                result.extend(collect_checked_nodes(child, current_path))
                # 移动到下一个兄弟节点
                child = child.getNext()
                
            # 返回收集到的路径列表
            return result
        
        # 构建结果数据结构
        def build_data_from_paths(paths):
            # 初始化结果字典
            result = {}
            
            # 遍历所有收集到的路径
            for path in paths:
                # 跳过只有根节点的路径（长度小于等于1）
                if len(path) <= 1:
                    continue
                    
                # 提取除根节点外的路径部分
                node_path = path[1:]
                
                # 获取材料名称（路径的第一个元素）
                material_name = path[0]
                # 如果材料名称不在materials_data中，则跳过
                if material_name not in self.materials_data:
                    continue
                    
                # 获取对应材料的原始数据源
                data_source = self.materials_data[material_name]
                
                # 遍历路径获取数据
                temp_data = data_source
                # 遍历路径中除最后一个键之外的所有键
                for i, key in enumerate(node_path[:-1]):
                    # 如果键不存在于当前数据中，则中断循环
                    if key not in temp_data:
                        break
                    # 更新temp_data为子字典
                    temp_data = temp_data[key]
                
                # 检查最后一个键是否存在于temp_data中
                last_key = node_path[-1]
                if last_key not in temp_data:
                    continue
                
                # 构建结果数据结构
                temp_dict = result
                # 遍历路径中除最后一个键之外的所有键，创建嵌套字典
                for i, key in enumerate(node_path[:-1]):
                    if key not in temp_dict:
                        temp_dict[key] = {}
                    temp_dict = temp_dict[key]
                
                # 添加叶子节点的数据到结果字典中
                temp_dict[last_key] = temp_data[last_key]
            
            # 返回构建好的数据字典
            return result
        
        # 获取树形视图的第一个根节点
        root = self.tree.getFirstItem()
        # 如果没有根节点，则返回空字典
        if not root:
            return jsondata
        
        # 收集所有勾选的路径
        checked_paths = collect_checked_nodes(root)
        # 将勾选的路径输出到消息区域，用于调试
        mw.writeToMessageArea("Checked paths: " + str(checked_paths))
        
        # 从收集到的路径构建最终的数据字典
        jsondata = build_data_from_paths(checked_paths)
        
        # 添加调试输出
        try:
            # 将字典转换为字符串并安全输出到消息区域
            json_str = str(jsondata)
            mw.writeToMessageArea("Selected data: " + json_str)
        except Exception as e:
            # 如果转换或输出过程中发生错误，则输出错误信息
            mw.writeToMessageArea("Error displaying data: " + str(e))
        
        # 返回最终的JSON数据
        return jsondata
    def onParamNameDblClicked(self, sender, sel, ptr):
        """双击‘参数名’列：进入草图并聚焦到该参数的尺寸（安全精简版）"""
        try:
            tbl = self.getTable()
            row = tbl.getCurrentRow()
            col = tbl.getCurrentColumn()
            if row <= 0 or col != 1:
                return

            param_name   = tbl.getItemText(row, 1).strip()
            part_name    = tbl.getItemText(row, 4).strip()
            feature_name = tbl.getItemText(row, 5).strip()
            if not part_name or not feature_name:
                getAFXApp().getAFXMainWindow().writeToMessageArea(
                    u"跳转失败：缺少部件/特征".encode('GB18030'))
                return

            # 逐行拼脚本，避免 % 格式化冲突 & 顶格无缩进
            cmd = "\n".join([
                "from abaqus import mdb, session",
                "from abaqusConstants import *",
                "import re",
                "vp = session.currentViewportName",
                "partName = {!r}".format(part_name),
                "featName = {!r}".format(feature_name),
                "pModel = None",
                "for _mn,_m in mdb.models.items():",
                "    if partName in _m.parts: pModel=_mn; break",
                "if pModel is None: raise RuntimeError('Part not found: %s' % partName)",
                "m = mdb.models[pModel]",
                "p = m.parts[partName]",
                "keys = [k for k in p.features.keys() if k.upper()==featName.upper()]",
                "if not keys: raise RuntimeError('Feature not found: %s' % featName)",
                "fk = keys[0]",
                "if '__edit__' in m.sketches:",
                "    try: m.sketches['__edit__'].unsetPrimaryObject()",
                "    except: pass",
                "    del m.sketches['__edit__']",
                "s0 = p.features[fk].sketch",
                "m.ConstrainedSketch(name='__edit__', objectToCopy=s0)",
                "s = m.sketches['__edit__']",
                "s.setPrimaryObject(option=SUPERIMPOSE)",
                "p.projectReferencesOntoSketch(sketch=s, upToFeature=p.features[fk], filter=COPLANAR_EDGES)",
                "session.viewports[vp].setValues(displayedObject=s)",
                "",
                "# 用参数反查尺寸索引（最稳妥的做法）",
                "paramName = {!r}".format(param_name),
                "try:",
                "    prm = s.parameters[paramName]",
                "    mobj = re.search(r'dimensions\\[(\\d+)\\]', prm.path)",
                "    if not mobj: raise KeyError",
                "    didx = int(mobj.group(1))",
                "    d = s.dimensions[didx]",
                "    # 若有文本点就用其做视图中心；否则全图",
                "    try:",
                "        x,y = map(float, d.textPoint)",
                "        v = session.viewports[vp].view",
                "        # 只缩放宽高到当前的 0.35，避免改 cameraPosition（更稳）",
                "        v.setValues(cameraTarget=(x, y, 0.0))",
                "        v.setValues(width=max(v.width*0.35, 10.0), height=max(v.height*0.35, 10.0))",
                "    except:",
                "        session.viewports[vp].view.fitView()",
                "except Exception as _e:",
                "    # 找不到尺寸也别崩，直接自适应视图",
                "    session.viewports[vp].view.fitView()",
            ])

            sendCommand(cmd)

        except Exception as e:
            getAFXApp().getAFXMainWindow().writeToMessageArea(
                u"跳转失败: {}".format(unicode(str(e), 'utf-8', 'replace')).encode('GB18030'))
    # 定义处理“更新模型”按钮点击事件的方法
    def onUpdateModelClicked(self, sender, sel, ptr):
        # 获取Abaqus主窗口实例
        mw = getAFXApp().getAFXMainWindow()
        # 获取Excel文件路径，并去除首尾空格
        excel_path = self.fileNameKw.getValue().strip()          
        # 检查Excel文件路径是否为空
        if not excel_path:
            # 如果为空，显示错误对话框并返回
            showAFXErrorDialog(mw, u"请先指定 Excel 文件路径")
            return
        # 将Excel文件路径转换为绝对路径
        excel_path = os.path.abspath(excel_path)
        # 获取工作表名称，并去除首尾空格
        sheet_name = self.ComboBox_14.getText().strip()
        # 将工作表名称输出到消息区域，用于调试
        mw.writeToMessageArea("Sheet name: " + str(sheet_name))
        # 检查工作表名称是否为空
        if not sheet_name:
            # 如果为空，显示错误对话框并返回
            showAFXErrorDialog(mw, u"请先选择 / 输入工作表名称")
            return
        # 获取当前插件的目录
        plugin_dir = os.path.dirname(__file__)
        # 构建要执行的Python命令字符串
        cmd = (
            "import sys, os\n" # 导入sys和os模块
            "sys.path.insert(0, r'{plugin}')\n" # 将插件目录添加到Python路径
            "import Parametric_modeling as pm\n" # 导入Parametric_modeling模块并别名为pm
            "pm.pre_paraModeling_main(r'{xls}', r'{sheet}')\n" # 调用pm模块中的主函数
        ).format(
            plugin=plugin_dir.replace('\\', '\\\\'), # 格式化插件路径，转义反斜杠
            xls=excel_path.replace('\\', '\\\\'), # 格式化Excel路径，转义反斜杠
            sheet=sheet_name # 格式化工作表名称
        )
        # 输出命令开始标记到消息区域
        mw.writeToMessageArea("===== CMD START =====\n")
        # 输出构建的命令字符串到消息区域
        mw.writeToMessageArea(cmd)
        # 输出命令结束标记到消息区域
        mw.writeToMessageArea("===== CMD END   =====\n")
        # 执行构建的Python命令
        sendCommand(cmd)
        # 输出模型更新成功的消息到消息区域，并进行编码以支持中文
        mw.writeToMessageArea(
            (u"模型尺寸已根据 {file} | Sheet: {sheet} 更新完毕\n"
            .format(file=os.path.basename(excel_path), sheet=sheet_name))
            .encode('GB18030'))


    # 定义处理“导入1”按钮点击事件的方法
    def onImport1Clicked(self, sender, sel, ptr):
        """处理import1按钮点击事件"""
        try:
            # 获取Abaqus主窗口实例
            mw = getAFXApp().getAFXMainWindow()
            
            # 获取材料名称并验证 (以下是被注释掉的旧代码，用于直接从文本框获取)
            # aimMaterialName = self.newMaterialText.getText()
            # if not aimMaterialName or not aimMaterialName.strip():
            #     mw.writeToMessageArea("Error: Material name cannot be empty")
            #     return
                
            # 验证材料名称是否合法（只允许字母、数字、下划线和中文）(以下是被注释掉的旧代码)
            # import re
            # if not re.match(r'^[a-zA-Z0-9_\u4e00-\u9fa5]+$', aimMaterialName):
                # mw.writeToMessageArea("Error: Material name can only contain letters, numbers, underscores and Chinese characters")
                # return
            
            # mw.writeToMessageArea("Material name: " + str(aimMaterialName))
            # 获取ComboBox_15当前选中项的索引
            cur_item = self.ComboBox_15.getCurrentItem()
            # 获取ComboBox_15当前选中项的文本值
            combo_value = self.ComboBox_15.getItemText(cur_item)
            # 如果下拉框选择的是“New”
            if combo_value == "New":
            # 选择了“New”才用文本框
                # 从newMaterialText文本框获取新材料名称，并去除首尾空格
                aimMaterialName = self.newMaterialText.getText().strip()
                # 如果新材料名称为空，则显示错误对话框并返回
                if not aimMaterialName:
                    showAFXErrorDialog(mw, "Please input a new material name.")
                    return
            else:
                # 否则用下拉框的已有材料名
                # 如果不是“New”，则直接使用下拉框选中的材料名称
                aimMaterialName = combo_value
            # 获取UVARM（用户自定义状态变量）的数量
            UVARMnum = self.form.keyword61Kw.getValue() 
            # 获取SDV（状态相关变量）的数量
            SDVnum = self.form.keyword62Kw.getValue()
            # 将UVARM和SDV数量输出到消息区域
            mw.writeToMessageArea("UVARM count: " + str(UVARMnum) + ", SDV count: " + str(SDVnum))
            
            # 获取选中的材料数据
            mw.writeToMessageArea("Getting selected material data...")
            # 调用get_checked_data方法获取树形视图中勾选的数据
            jsondata = self.get_checked_data()
            
            # 检查jsondata的内容
            # 验证jsondata是否为字典类型
            if not isinstance(jsondata, dict):
                # 如果不是字典，输出错误信息并返回
                mw.writeToMessageArea("Error: JSON data must be a dictionary")
                return
                
            # 检查是否有选中的数据
            # 如果jsondata为空字典，表示没有选中任何数据
            if not jsondata:
                # 输出错误信息并返回
                mw.writeToMessageArea("Error: No material data selected")
                return
            
            # 输出选中的数据结构，便于调试
            try:
                # 将jsondata的所有键转换为字符串列表，并输出到消息区域
                keys_str = str(list(jsondata.keys()))
                mw.writeToMessageArea("Selected material data structure: " + keys_str)
            except Exception as e:
                # 如果输出键时发生错误，则输出错误信息
                mw.writeToMessageArea("Error displaying keys: " + str(e))
            
            # 检查数据结构的深度
            # 定义一个递归函数，用于检查并输出数据结构的深度和路径
            def check_data_depth(data, path=""):
                # 如果数据是字典类型
                if isinstance(data, dict):
                    # 遍历字典中的所有键值对
                    for key, value in data.items():
                        try:
                            # 构建新的路径字符串
                            new_path = path + " > " + str(key) if path else str(key)
                            # 输出当前数据路径
                            mw.writeToMessageArea("Data path: " + new_path)
                            # 递归调用自身，处理子数据
                            check_data_depth(value, new_path)
                        except Exception as e:
                            # 如果处理路径时发生错误，则输出错误信息
                            mw.writeToMessageArea("Error processing path: " + str(e))
            
            # 输出数据结构的深度信息
            mw.writeToMessageArea(u"数据结构细节:".encode('GB18030'))
            # 调用check_data_depth函数，从jsondata开始检查
            check_data_depth(jsondata)
            
            # 调用pre_materialImport导入材料
            # try:
            # 输出开始导入材料的消息
            mw.writeToMessageArea(u"开始材料导入...".encode('GB18030'))
            # 构建用于执行材料导入的Python命令字符串
            cmds=("import updatematerial\n" # 导入updatematerial模块
                    "updatematerial.pre_materialImport_main({},'{}',{},{})\n".format(jsondata, str(aimMaterialName), int(UVARMnum), int(SDVnum))) # 调用主导入函数
            # 执行构建的Python命令
            sendCommand(cmds)
            #fortran_data = self.pre_materialImport(jsondata, str(aimMaterialName), int(UVARMnum), int(SDVnum))
            # 导入成功后显示消息
            mw.writeToMessageArea(u"材料导入完成。".encode('GB18030'))
# 将材料名称格式化并编码为GB18030，然后写入消息区域，表示材料成功导入。
            # 调用方法更新界面上的材料下拉框，使其显示最新的材料列表。
            self.updateComboBox_15Materials()
            # 以下是被注释掉的代码块，原意是捕获导入材料时的特定异常。
            # except Exception as e:
            #     mw.writeToMessageArea("Error: Material import failed - " + str(e))
            #     return
            
        # 捕获导入材料过程中 发生的任何异常。
        except Exception as e:
            # 获取Abaqus/CAE应用程序的主窗口实例。
            mw = getAFXApp().getAFXMainWindow()
            # 向消息区域写入错误信息，指出材料导入失败，并包含具体的错误详情。
            # 错误信息被编码为GB18030以确保中文显示正确。
            mw.writeToMessageArea(u"错误: 材料导入失败 - {}".format(unicode(str(e), 'utf-8', errors='replace')).encode('GB18030'))
            # 导入traceback模块，用于获取更详细的错误堆栈信息。
            import traceback
            # 获取当前异常的完整堆栈跟踪信息。
            error_trace = str(traceback.format_exc())
            # 将详细的错误堆栈信息写入消息区域，帮助用户或开发者调试问题。
            mw.writeToMessageArea(u"详细错误信息: {}".format(unicode(error_trace, 'utf-8', errors='replace')).encode('GB18030'))

    # 定义Clicked_amplitude方法，当用户点击导入幅值表按钮时触发。
    # sender, sel, ptr是事件回调的标准参数。
    def Clicked_amplitude(self, sender, sel, ptr):
        # 获取Abaqus/CAE应用程序的主窗口实例。
        mw = getAFXApp().getAFXMainWindow()
        # 向消息区域写入提示信息，表示正在导入幅值表。
        mw.writeToMessageArea(u"正在导入幅值表...".encode('GB18030'))
        # 从表单的InputDataNameKw字段获取用户选择的文件路径。
        file_path = self.form.InputDataNameKw.getValue()
        
        # 检查文件路径是否为空，即用户是否选择了文件。
        if not file_path:
            # 如果文件路径为空，向消息区域写入错误信息。
            mw.writeToMessageArea(u"错误: 未选择文件".encode('GB18030'))
            # 终止方法执行。
            return
        
        # 尝试构建并执行导入幅值表的命令。
        try:
            # 构建Python命令字符串，该命令会导入fuzhijiemian模块，并调用其pre_ampTableChange函数。
            # pre_ampTableChange函数用于处理幅值表导入逻辑，并使用文件路径和特定占位符。
            cmds=("import fuzhijiemian\n"
                  "fuzhijiemian.pre_ampTableChange('{}','%OP%_%TT%')\n".format(file_path))
            # 发送构建好的命令到Abaqus/CAE的命令处理器执行。
            sendCommand(cmds)
            # 导入成功后，向消息区域写入成功提示信息。
            mw.writeToMessageArea(u"幅值表导入成功，请查看消息区域获取详细信息".encode('GB18030'))
        # 捕获导入幅值表过程中 发生的任何异常。
        except Exception as e:
            # 向消息区域写入错误信息，指出导入幅值表时出错，并包含具体的错误详情。
            mw.writeToMessageArea(u"导入幅值表时出错: {}".format(unicode(str(e), 'utf-8', errors='replace')).encode('GB18030'))
    
    # 定义Createstep方法，当用户点击创建分析步按钮时触发。
    # sender, sel, ptr是事件回调的标准参数。
    def Createstep(self, sender, sel, ptr):
        # 获取Abaqus/CAE应用程序的主窗口实例。
        mw = getAFXApp().getAFXMainWindow()
        # 向消息区域写入提示信息，表示开始创建分析步。
        mw.writeToMessageArea(u"开始创建分析步...".encode('GB18030'))
        
        # 尝试执行创建分析步的逻辑。
        try:
            # 从表单的keyword68Kw字段获取前置分析步的文本。
            bstep_text = self.form.keyword68Kw.getValue()
            # 从表单的keyword65Kw字段获取循环分析步的文本。
            csteplist_text = self.form.keyword65Kw.getValue()
            # 从表单的keyword69Kw字段获取后置分析步的文本。
            astep_text = self.form.keyword69Kw.getValue()
            # 从表单的keyword77Kw字段获取循环次数，并转换为整数。
            cyctimes = int(self.form.keyword77Kw.getValue())
            # 从表单的keyword90Kw字段获取模型类型，并转换为字符串。
            modeltype = str(self.form.keyword90Kw.getValue())
            
            # 解析前置分析步文本，按逗号分割并去除空白，如果为空则为[]。
            bstep = [x.strip() for x in bstep_text.split(',')] if bstep_text.strip() else []
            # 解析循环分析步文本，按逗号分割并去除空白，如果为空则为[]。
            csteplist = [x.strip() for x in csteplist_text.split(',')] if csteplist_text.strip() else []
            # 解析后置分析步文本，按逗号分割并去除空白，如果为空则为[]。
            astep = [x.strip() for x in astep_text.split(',')] if astep_text.strip() else []
            
            # 检查循环分析步列表是否为空。
            if not csteplist:
                # 如果为空，向消息区域写入错误信息。
                mw.writeToMessageArea(u"错误: 循环分析步不能为空".encode('GB18030'))
                # 终止方法执行。
                return
                
            # 获取步长字典的引用。
            steptimepair = self.steptimepair
            # 从表单的keyword94Kw字段获取HOLDING步的步长值，并更新到字典中。
            steptimepair['HOLDING'] = self.form.keyword94Kw.getValue()
            # 向消息区域打印当前的步长字典内容，用于调试。
            mw.writeToMessageArea(u'步长字典: {}'.format(steptimepair).encode('GB18030'))
            # 以下是被注释掉的默认步长字典示例。
            # {
            #     'G5':107600.0,
            #     'Steady':480000.0,
            #     'G6':3000.0,
            #     'G13':4500.0,
            #     'G9':18000.0,
            #     'HOLDING':20.0*365*24*3600,
            #     '*': 1.0#缺省值
            #     }  # 默认值
            
            # 以下是被注释掉的关于HOLDING步长和默认步长设置的代码。
            # # 添加HOLDING步长（如果有指定）
            # holding_time = self.form.keyword94Kw.getValue()
            # if holding_time and holding_time.strip():
            #     try:
            #         steptimepair['HOLDING'] = float(holding_time)
            #     except ValueError:
            #         mw.writeToMessageArea(u"警告: HOLDING步长格式不正确，将使用默认值".encode('GB18030'))
            #         steptimepair['HOLDING'] = 20.0*365*24*3600  # 默认20年
            # else:
            #     steptimepair['HOLDING'] = 20.0*365*24*3600  # 默认20年
                
            # # 为csteplist中的每个步骤设置默认步长
            # for step in csteplist:
            #     if step not in steptimepair:
            #         steptimepair[step] = 1.0
                    
            # # 为bstep和astep中的每个步骤设置默认步长
            # for step in bstep + astep:
            #     if step not in steptimepair:
            #         steptimepair[step] = 1.0
            
            # 根据HFrame33Kw1的值判断创建标志是'REPLACE'（替换）还是'NEW'（新建）。
            creatFlag = 'REPLACE' if self.form.HFrame33Kw1.getValue() == 59 else 'NEW'
            
            # 构建Python命令字符串，该命令会导入fenxibu模块，并调用其pre_stepBuild函数。
            # pre_stepBuild函数用于根据提供的参数创建分析步。
            cmds=("import fenxibu\n"
                  "fenxibu.pre_stepBuild({},{},{},{},{},'{}','{}')\n".format(bstep, csteplist, steptimepair, astep, cyctimes, modeltype, creatFlag))
            # 发送构建好的命令到Abaqus/CAE的命令处理器执行。
            sendCommand(cmds)
            
            # 向消息区域写入成功提示信息，表示分析步创建成功。
            mw.writeToMessageArea(u"分析步创建成功!".encode('GB18030'))

        # 捕获创建分析步过程中 发生的任何异常。
        except Exception as e:
            # 向消息区域写入错误信息，指出创建分析步时出错，并包含具体的错误详情。
            mw.writeToMessageArea(u"创建分析步时出错: '{}'".format(unicode(str(e), 'utf-8', errors='replace')).encode('GB18030'))
            # 再次导入traceback模块，以防之前未导入。
            error_trace = traceback.format_exc()
            # 将详细的错误堆栈信息写入消息区域。
            mw.writeToMessageArea(error_trace)

    # 定义Modifystep方法，当用户点击修改分析步按钮时触发。
    # sender, sel, ptr是事件回调的标准参数。
    def Modifystep(self, sender, sel, ptr):
        # 获取Abaqus/CAE应用程序的主窗口实例。
        mw = getAFXApp().getAFXMainWindow()
        # 向消息区域写入提示信息，表示开始修改分析步。
        mw.writeToMessageArea(u"开始修改分析步...".encode('GB18030'))
        
        # 尝试执行修改分析步的逻辑。
        try:
            # 获取当前激活的模型。
            m = self.get_current_model()
            # 如果没有找到当前模型。
            if not m:
                # 向消息区域写入错误信息。
                mw.writeToMessageArea(u"错误: 未找到当前模型".encode('GB18030'))
                # 终止方法执行。
                return
                
            # 获取当前模型中所有分析步的名称列表。
            all_steps = list(m.steps.keys())
            # 如果当前模型没有分析步。
            if not all_steps:
                # 向消息区域写入错误信息。
                mw.writeToMessageArea(u"错误: 当前模型没有分析步".encode('GB18030'))
                # 终止方法执行。
                return
                
            # 获取用户在GroupBox23Kw1单选框中的选择。
            radio_selection = self.form.GroupBox23Kw1.getValue()
            
            # 获取界面中的其他参数。
            # 获取修改方法（例如，时间、增量步等）。
            edittype = self.form.keyword70Kw.getValue()  # 方法
            # 获取要修改的值。
            value = self.form.keyword74Kw.getValue()     # 值
            
            # 初始化一个空列表，用于存储筛选后的分析步。
            filtered_steps = []
            
            # 根据单选框的选择进行分析步筛选。
            if radio_selection == 60:  # 第一个单选框 - 对名字中含有...的分析步
                # 获取用户输入的筛选文本。
                filter_text = self.form.keyword72Kw.getValue()
                # 如果筛选文本为空。
                if not filter_text:
                    # 向消息区域写入警告信息，表示将修改所有分析步。
                    mw.writeToMessageArea(u"警告: 未指定筛选文本，将修改所有分析步".encode('GB18030'))
                    # 将所有分析步都添加到筛选列表中。
                    filtered_steps = all_steps
                else:
                    # 筛选包含指定文本的分析步。
                    filtered_steps = [step for step in all_steps if filter_text in step]
                    # 向消息区域报告筛选结果。
                    mw.writeToMessageArea(u"根据文本 '{}' 筛选出 {} 个分析步".format(
                        filter_text, len(filtered_steps)).encode('GB18030'))
            
            # 如果选择的是第二个单选框 - 对于从i开始每n的分析步(n,i)。
            elif radio_selection == 61:  # 第二个单选框 - 对于从i开始每n的分析步(n,i)
                # 获取用户输入的模式文本（n,i）。
                pattern_text = self.form.keyword73Kw.getValue()
                # 如果模式文本为空。
                if not pattern_text:
                    # 向消息区域写入错误信息。
                    mw.writeToMessageArea(u"错误: 未指定 n,i 格式".encode('GB18030'))
                    # 终止方法执行。
                    return
                    
                # 尝试解析n和i的值。
                try:
                    # 将模式文本按逗号分割。
                    parts = pattern_text.split(',')
                    # 如果分割后的部分少于2个。
                    if len(parts) < 2:
                        # 向消息区域写入错误信息，提示正确的格式。
                        mw.writeToMessageArea(u"错误: 格式应为 'n,i'".encode('GB18030'))
                        # 终止方法执行。
                        return
                        
                    # 将第一个部分解析为整数n（每n个）。
                    n = int(parts[0].strip())  # 每n个
                    # 将第二个部分解析为整数i（从i开始）。
                    i = int(parts[1].strip())  # 从i开始
                    
                    # 确保i至少为1。
                    if i < 1:
                        i = 1  # 确保i至少为1
                        
                    # 按照从i开始每n个的方式筛选分析步。
                    filtered_steps = []
                    # 遍历所有分析步，同时获取索引（从1开始计数）。
                    for idx, step in enumerate(all_steps, 1):  # 从1开始计数
                        # 如果当前索引满足 (idx - i) % n == 0 且 idx >= i 的条件，则添加到筛选列表。
                        if (idx - i) % n == 0 and idx >= i:
                            filtered_steps.append(step)
                    
                    # 向消息区域报告筛选结果。
                    mw.writeToMessageArea(u"从第{}个开始每{}个分析步，筛选出{}个分析步".format(
                        i, n, len(filtered_steps)).encode('GB18030'))
                        
                # 捕获值转换错误（例如，n或i不是整数）。
                except ValueError:
                    # 向消息区域写入错误信息。
                    mw.writeToMessageArea(u"错误: n,i 必须是整数".encode('GB18030'))
                    # 终止方法执行。
                    return
            # 如果没有选择任何筛选方式。
            else:
                # 将所有分析步都添加到筛选列表中。
                filtered_steps = all_steps
                # 向消息区域写入提示信息。
                mw.writeToMessageArea(u"未指定筛选方式，将修改所有分析步".encode('GB18030'))
            
            # 检查筛选结果是否为空。
            if not filtered_steps:
                # 如果筛选后没有分析步符合条件，向消息区域写入警告信息。
                mw.writeToMessageArea(u"警告: 筛选后没有分析步符合条件".encode('GB18030'))
                # 终止方法执行。
                return
                
            # 检查编辑类型是否为空。
            if not edittype:
                # 如果编辑类型为空，向消息区域写入错误信息。
                mw.writeToMessageArea(u"错误: 请选择修改方法".encode('GB18030'))
                # 终止方法执行。
                return
                
            # 构建一个字符串，表示筛选后的分析步名称列表，格式为Python列表字符串。
            step_names_str = "[" + ",".join(["'{}'".format(step) for step in filtered_steps]) + "]"
            
            # 确保值是正确格式，根据其类型决定是否加引号。
            if value.isdigit() or (value and value[0] == '-' and value[1:].isdigit()):
                value_str = value  # 如果是纯数字或负数数字字符串，则不加引号。
            else:
                try:
                    float_val = float(value)
                    value_str = str(float_val)  # 如果能转换为浮点数，则转换为字符串形式（不加引号）。
                except ValueError:
                    value_str = "'{}'".format(value)  # 否则，视为字符串并加引号。
            
            # 构建Python命令字符串，该命令会导入fenxibu模块，并调用其pre_stepModify函数。
            # pre_stepModify函数用于修改指定的分析步的属性。
            cmds = ("import fenxibu\n"
                   "fenxibu.pre_stepModify({}, '{}', {})\n".format(
                       step_names_str, edittype, value_str))
            
            # 调试输出命令，将要执行的命令写入消息区域。
            mw.writeToMessageArea(u"执行命令: {}".format(cmds).encode('GB18030'))
            
            # 执行构建好的命令。
            sendCommand(cmds)
            
            # 向消息区域写入成功提示信息，表示分析步修改成功。
            mw.writeToMessageArea(u"分析步修改成功!".encode('GB18030'))
            
        # 捕获修改分析步过程中 发生的任何异常。
        except Exception as e:
            # 向消息区域写入错误信息，指出修改分析步时出错，并包含具体的错误详情。
            mw.writeToMessageArea(u"修改分析步时出错: '{}'".format(unicode(str(e), 'utf-8', errors='replace')).encode('GB18030'))
            # 导入traceback模块，以防之前未导入。
            import traceback
            # 获取当前异常的完整堆栈跟踪信息。
            error_trace = traceback.format_exc()
            # 将详细的错误堆栈信息写入消息区域。
            mw.writeToMessageArea(error_trace)

    # 定义onMaterialChanged方法，当材料选择下拉框的值改变时触发。
    def onMaterialChanged(self, sender, sel, ptr):
        # 获取ComboBox_12（材料下拉框）中当前选中的材料名称。
        selected_material = self.ComboBox_12.getItemText(self.ComboBox_12.getCurrentItem())
        # 调用updateTree方法，根据选中的材料更新树形结构显示。
        self.updateTree(selected_material)

    # 定义updateMaterialComboBox方法，用于更新材料下拉框的内容。
    def updateMaterialComboBox(self, materials):
        # 清空ComboBox_12（材料下拉框）中现有的所有项。
        self.ComboBox_12.clearItems()
        # 遍历传入的材料列表。
        for material in materials:
            # 将每个材料名称添加为下拉框的一个新项。
            self.ComboBox_12.appendItem(text=material)

    # 定义onItemDoubleClicked方法，当树形结构中的项被双击时触发。
    def onItemDoubleClicked(self, sender, sel, ptr):
        # 定义一个嵌套函数get_item_data，用于从树形项获取其对应的数据。
        def get_item_data(item):
            # 初始化一个空列表来存储从当前项到根项的路径。
            path = []
            # 循环向上遍历，直到达到根项（getParent()返回None）。
            while item is not None:
                # 将当前项的文本（名称）添加到路径列表中。
                path.append(self.tree.getItemText(item))
                # 获取当前项的父项。
                item = item.getParent()
            # 反转路径列表，使其从根项开始。
            path.reverse()
            # 从存储的材料数据中获取根数据。
            data = self.materials_data
            
            # 注释：self.materials_data是一个字典，通过路径逐级访问其内部数据。
            # self.materials_data【path1】【path2】【path3】=
            # 遍历路径中的每个级别。
            for level in path:
                # 尝试从当前数据中获取下一级别的数据。
                data = data.get(level)
                # 如果任何级别的数据不存在，则返回None。
                if data is None:
                    return None
            # 返回最终获取到的数据。
            return data
        
        # 以下是被注释掉的调试代码，用于在消息区域打印信息。
        # mw = getAFXApp().getAFXMainWindow()
        # mw.writeToMessageArea('TreeItemDoubleClicked1')
        # 获取当前被选中的树形项。
        selected_item = self.tree.getCurrentItem()
        # 如果有项被选中。
        if selected_item:
            # 获取选中项的名称。
            node_name = self.tree.getItemText(selected_item)
            # 获取选中项对应的数据。
            selected_item_data = get_item_data(selected_item)
            # 如果获取到的数据是列表类型（通常表示这是一个可编辑的属性）。
            if isinstance(selected_item_data, list):
                # 调用showDetailDialog方法显示详细信息对话框，允许用户编辑数据。
                self.showDetailDialog(node_name, selected_item)
            # 以下是被注释掉的调试代码，用于显示信息对话框。
            # showAFXInformationDialog(mw, str(get_item_path(selected_item)))

    # 以下是被注释掉的旧版showDetailDialog方法，用于显示只读详情。
    # def showDetailDialog(self, node_name, material_data):
    #     detail_dialog = AFXDialog(self, "Details for " + node_name, opts=DIALOG_NORMAL, w=0,
    #                               h=0)
    #     vframe = FXVerticalFrame(detail_dialog, opts=LAYOUT_FILL_X | LAYOUT_FILL_Y)
    #     # for key, value in material_data.items():
    #     #     label = FXLabel(vframe, "{}: {}".format(key, str(value)))
    #     label = FXLabel(vframe, "{}: {}".format(node_name, str(material_data)))
    #     detail_dialog.create()
    #     detail_dialog.show()
    # 定义showDetailDialog方法，用于显示材料数据的详细编辑对话框。
    def showDetailDialog(self, node_name, material_item):
        # 创建MaterialDataDialog实例，传入父窗口、对话框标题和材料项数据。
        dialog = MaterialDataDialog(self, "Editing: {}".format(node_name), material_item)
        # 创建对话框的GUI元素。
        dialog.create()
        # 以模态方式显示对话框，这意味着用户必须关闭此对话框才能与主应用程序交互。
        dialog.showModal()  # 模态显示
        
        # 对话框关闭后，检查是否有修改过的数据。
        if dialog.getModifiedData() is not None:
            # 如果有修改，更新临时JSON变量（self.temp_json）中对应节点的数据。
            # 这里的temp_json是一个示例变量，实际 对应存储材料数据的结构。
            self.temp_json[node_name] = dialog.getModifiedData()
            
            # 可选：立即更新树形结构显示，以反映数据的变化。
            self.updateTree(self.ComboBox_12.getItemText(
                self.ComboBox_12.getCurrentItem()))
    ###########################################################################
    
    
    # 定义show方法，当对话框被显示时调用。
    def show(self):
        """显示对话框时被调用"""
        # 调用父类的show方法来显示对话框。
        AFXDataDialog.show(self)

        # 注册一个会话变化监听器，当Abaqus会话发生变化时，会触发onSessionChange方法。
        session.registerQuery(self.onSessionChange, False)
        
        # 获取当前上下文中的模型名称。
        currentModelName = getCurrentContext().get('modelName', '')
        # 检查获取到的模型名称是否存在于mdb（模型数据库）中。
        if currentModelName in mdb.models:
            # 如果存在，将当前模型对象赋值给self.regModel。
            self.regModel = mdb.models[currentModelName]
            # 以下是被注释掉的材料变化监听器注册代码。
            # self.regModel.materials.registerQuery(self.updateComboBox_15Materials, False)
            # 将当前模型名称设置到表单的keyword99Kw字段中。
            self.form.keyword99Kw.setValue(currentModelName)
        
        # 更新材料下拉框，确保其显示当前模型的材料列表。
        self.updateComboBox_15Materials()

    # 定义hide方法，当对话框被隐藏时调用。
    def hide(self):
        """隐藏对话框时被调用"""
        # 调用父类的hide方法来隐藏对话框。
        AFXDataDialog.hide(self)
        # 注销会话变化监听器，防止在对话框隐藏后继续监听。
        session.unregisterQuery(self.onSessionChange)
        # 尝试注销材料变化监听器。
        try:
            # 如果self.regModel存在（即有注册的模型）。
            if self.regModel:
                # 注销材料变化监听器，防止在对话框隐藏后继续监听材料变化。
                self.regModel.materials.unregisterQuery(self.updateComboBox_15Materials)
        # 捕获注销过程中 发生的任何异常，并忽略它们。
        except:
            pass

    # 定义updateComboBox_15Materials方法，用于更新ComboBox_15（另一个材料下拉框）的内容。
    def updateComboBox_15Materials(self):
        # 获取当前上下文中的模型名称。
        currentModelName = getCurrentContext().get('modelName', '')
        # 检查获取到的模型名称是否存在于mdb（模型数据库）中。
        if currentModelName in mdb.models:
            # 清空ComboBox_15中现有的所有项。
            self.ComboBox_15.clearItems()

            # 获取当前模型对象。
            model  = mdb.models[currentModelName]
            # 获取模型中所有材料的名称，并进行排序。
            names  = sorted(model.materials.keys())   
            # 遍历排序后的材料名称列表。
            for name in names:
                # 将每个材料名称添加为下拉框的一个新项。
                self.ComboBox_15.appendItem(name)

            # 在材料列表的末尾添加一个“New”选项，允许用户创建新材料。
            self.ComboBox_15.appendItem("New")

            # 如果存在材料名称。
            if names:
                # 将第一个材料名称设置为默认选中项。
                default = names[0]
            # 如果没有材料名称。
            else:
                # 将默认值 "New" 设置给表单中的 keyword98Kw 控件。
                default= "New"
            self.form.keyword98Kw.setValue(default)

            # 调整窗口大小到默认宽度和高度。
            self.resize(self.getDefaultWidth(), self.getDefaultHeight())
            # 返回 1 表示操作成功。
            return 1
        # 返回 0 表示操作失败。
        return 0

    def onSessionChange(self):
        # 此方法在会话发生改变时被调用，包括模型切换。
        """当会话改变时（包括模型切换）被调用"""
        # 获取当前模型的名称，如果不存在则为空字符串。
        currentModelName = getCurrentContext().get('modelName', '')
        # 检查当前模型名称是否存在于模型数据库中。
        if currentModelName in mdb.models:
            # 如果存在已注册的模型且其名称与当前模型名称不同，说明模型已切换。
            if self.regModel and getattr(self.regModel,'name',None) != currentModelName: #当模型切换过
                # 尝试取消注册旧模型的材料查询。
                # 取消注册旧模型材料
                try:
                    self.regModel.materials.unregisterQuery(self.updateComboBox_15Materials)
                except:
                    # 如果取消注册失败，则忽略错误。
                    pass
            # 注册新模型材料。
            # 注册新模型材料
            self.regModel = mdb.models[currentModelName]
            # self.regModel.materials.registerQuery(self.updateComboBox_15Materials, False) # 这行代码被注释掉了，表示不执行材料查询注册。
            
            # 更新界面上 ModelName 的显示。
            # 更新ModelName显示
            self.form.keyword99Kw.setValue(currentModelName)
            
            # 更新材料下拉框的内容。
            # 更新材料下拉框
            self.updateComboBox_15Materials()
            # 返回 1 表示操作成功。
            return 1
        else:
            # 如果当前模型名称不在模型数据库中，返回 0 表示操作失败。
            return 0

    def onSheetChanged(self, sender, sel, ptr, *args):
        # 当工作表选择改变时触发此方法。
        # 获取当前选中的 Sheet 索引
        selected_sheet_index = self.ComboBox_14.getCurrentItem()

        # 获取文件路径
        selected_file_path = self.fileNameKw.getValue()

        # 检查文件路径是否已选择。
        if selected_file_path:
            try:
                # 尝试打开 Excel 文件。
                # 打开 Excel 文件
                workbook = xlrd.open_workbook(selected_file_path)

                # 根据选中的索引获取对应的工作表。
                # 获取选中的 Sheet
                sheet = workbook.sheet_by_index(selected_sheet_index)

                # 使用获取到的工作表数据填充表格。
                # 填充表格数据
                self.fillTableWithSheetData(sheet)
            except Exception as e:
                # 如果读取工作表时发生错误，则在消息区域显示错误信息。
                mw = getAFXApp().getAFXMainWindow()
                mw.writeToMessageArea("Error reading selected sheet: " + str(e))

    def fillTableWithSheetData(self, sheet):
        # 此方法用于使用给定工作表的数据填充表格控件。
        # 获取表格控件的引用。
        table = self.getTable()

        # 清空表格中所有现有内容。
        # 清空表格内容
        for row in range(table.getNumRows()):
            for col in range(table.getNumColumns()):
                # 将每个单元格的内容设置为空字符串。
                table.setItemText(row, col, "")  # 将每个单元格内容设置为空字符串

        # 设置表格的行标题（表头），使用 GB18030 编码。
        # 设置表头
        table.setLeadingRowLabels(u'参数名\t参数值\t类型\t部件\t特征'.encode('GB18030'))

        # 遍历工作表中的每一行和每一列，将数据填充到表格中。
        # 填充数据
        for row in range(sheet.nrows):
            for col in range(sheet.ncols):
                # 获取当前单元格的值。
                value = sheet.cell_value(row, col)
                # 将值填充到表格中，注意表格的行和列索引从 1 开始（跳过表头）。
                table.setItemText(row + 1, col + 1, str(value))  # 从第二行开始填充数据

        # 更新表格显示，使其反映数据的变化。
        # 更新表格显示
        table.update()

    # ─── 仍然在 STPM_test1033DB 类体内，放在其他方法后面 ───
    # 此方法在主标签页切换时被调用。
    def onMainTabChanged(self, sender, sel, ptr):
        try:
            # 检查 sender 是否有效且是 FXTabBook 实例。
            if sender is None or not isinstance(sender, FXTabBook):
                # 如果不是，则直接返回。
                return
            # 如果当前选中的标签页是索引为 4 的页面（对应 Loads&HTC）。
            if sender.getCurrent() == 4:      # 4 对应 Loads&HTC
                # 将 Cycle 标签页中的关键字值同步到 Loads&HTC 标签页对应的关键字。
                # 从 Cycle 关键字 → Loads&HTC 关键字
                self.form.keyword92Kw.setValue(self.form.keyword68Kw.getValue())   # before
                self.form.keyword80Kw.setValue(self.form.keyword65Kw.getValue())   # composition
                self.form.keyword93Kw.setValue(self.form.keyword69Kw.getValue())   # after
                self.form.keyword81Kw.setValue(self.form.keyword77Kw.getValue())   # cycle times

                # 调用 onTextChanged 方法更新相关显示。
                self.onTextChanged(None, None, None)
                # 设置标志位 tab34flag 为 True。
                self.tab34flag=True
            # 如果当前选中的标签页是索引为 3 的页面，并且 tab34flag 为 True。
            elif sender.getCurrent() == 3 and self.tab34flag:
                # 重置标志位 tab34flag 为 False。
                self.tab34flag=False
                # 将 Loads&HTC 标签页中的关键字值同步回 Cycle 标签页对应的关键字。
                self.form.keyword68Kw.setValue(self.form.keyword92Kw.getValue())   # before
                self.form.keyword65Kw.setValue(self.form.keyword80Kw.getValue())   # composition
                self.form.keyword69Kw.setValue(self.form.keyword93Kw.getValue())   # after
                self.form.keyword77Kw.setValue(self.form.keyword81Kw.getValue())   # cycle times
        except Exception as e:
            # 如果同步过程中发生错误，则在消息区域显示错误信息。
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(u'同步 Cycle.Loads&HTC 失败: {}'.format(e).encode('gb18030'))

    def onListItemDoubleClicked(self, sender, sel, ptr, *args):
        # 此方法在列表项被双击时触发。
        # 获取当前选中的 item
        try:
            # 获取 List_3 控件中当前选中的项。
            selected_item = self.List_3.getCurrentItem()
            # 检查是否有项被选中（非空字符串）。
            if selected_item!="":
                # 获取选中项的文本内容。
                # 获取 item 的文本内容
                item_text = self.List_3.getItemText(selected_item)
    
                # 将 item 的文本内容写入 AFXTextField 控件。
                # 将 item 的文本内容写入 AFXTextField
                old_text=self.form.keyword65Kw.getValue()
                # 如果 AFXTextField 当前为空，则直接设置新值。
                if old_text=='':
                    self.form.keyword65Kw.setValue(str(item_text))
                else:
                    # 否则，将新值追加到现有值后面，用逗号分隔。
                    self.form.keyword65Kw.setValue(self.form.keyword65Kw.getValue()+','+str(item_text))
    
                # 以下是被注释掉的代码，用于在消息区域显示选中的内容。
                # 如果需要，可以在消息区域显示选中的内容
                # mw = getAFXApp().getAFXMainWindow()
                # mw.writeToMessageArea("Selected item: " + str(item_text))
        except Exception as e:
            # 如果发生错误，则在消息区域显示错误信息。
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(str(e))

    def rename_duplicates(self, lst):
        # 此方法用于重命名列表中重复的元素，为其添加后缀（如 item-1, item-2）。
        # 导入 Counter 和 defaultdict，确保它们在文件顶部被导入。
        from collections import Counter, defaultdict
        # 统计每个元素的总出现次数
        freq = Counter(lst)
        # 用于记录每个重复元素目前出现的次数
        occurrence = defaultdict(int)
        # 初始化结果列表。
        result = []
        # 遍历输入列表中的每个元素。
        for item in lst:
            # 如果元素出现多次，则添加后缀。
            if freq[item] > 1:
                # 增加该元素的出现次数计数。
                occurrence[item] += 1
                # 将带有后缀的元素添加到结果列表。
                result.append("{}-{}".format(item, occurrence[item]))
            else:
                # 如果元素不重复，则直接添加到结果列表。
                result.append(item)
        # 返回处理后的列表。
        return result

    def onTextChanged(self, sender, sel, ptr, *args):
        # 此方法在文本框内容改变时触发，用于更新表格。
        # 获取 AFXTextField (keyword80Kw) 的内容。
        # 获取 AFXTextField 的内容
        text = self.form.keyword80Kw.getValue()
        # 将内容按逗号分隔解析为列表。
        # 将内容解析为列表
        items = text.split(',')
        # 使用 rename_duplicates 函数处理列表，为重复项添加后缀。
        # 使用 rename_duplicates 函数修饰列表
        renamed_items = self.rename_duplicates(items)
        # 获取表格控件 table1 的引用。
        # 获取表格控件
        table = self.getTable1()
        # 清空表格中除第一行（表头）外的所有内容。
        # 清空表格内容
        for row in range(1, table.getNumRows()):  # 从第2行开始
            for col in range(1, table.getNumColumns()):  # 从第2列开始
                table.setItemText(row, col, "")
        # 遍历处理后的列表，将数据填充到表格中。
        # 填充表格
        for i, item in enumerate(renamed_items):
            # 填充第一列。
            # 第一列
            table.setItemText(i + 1, 1, item)

            # 填充第二列。
            # 第二列
            # 如果元素是 'HOLDING'，则第二列设置为 'Propagated'。
            if item == 'HOLDING':
                table.setItemText(i + 1, 2, 'Propagated')
            else:
                # 否则，设置为当前项的索引加 1。
                table.setItemText(i + 1, 2, str(i + 1))
            # 填充第三列，固定为 '0'。
            # 第三列
            table.setItemText(i + 1, 3, '0')
            # 填充第四列，设置为当前项的索引加 1。
            # 第四列
            table.setItemText(i + 1, 4, str(i + 1))
            # 填充第五列，固定为 '-1'。
            # 第五列
            table.setItemText(i + 1, 5, '-1')
        # 更新表格显示。
        # 更新表格显示
        table.update()

    def onCycleListChanged(self, sender, sel, ptr, *args):
        # 此方法在 Cycle 列表内容改变时触发。
        # 获取 keyword65Kw 控件的内容。
        # 获取控件1的内容
        cycle_list_text = self.form.keyword65Kw.getValue()
        # 将获取到的内容设置到 keyword80Kw 控件。
        # 将内容设置到控件2
        self.form.keyword80Kw.setValue(cycle_list_text)

    def getTable(self):
        # 返回对表格控件 self.table 的引用。
        return self.table

    def getComboBox14(self):
        # 返回对下拉框控件 self.ComboBox_14 的引用。
        return self.ComboBox_14

    def getList3(self):
        # 返回对列表控件 self.List_3 的引用。
        return self.List_3


    def getTable1(self):
        # 返回对表格控件 self.table1 的引用。
        return self.table1

    def onTabChanged(self, sender, sel, ptr):
        # 此方法在标签页切换时被调用。
        # 获取当前模型。
        m = self.get_current_model()
        # 获取 AFX 应用程序的主窗口。
        mw = getAFXApp().getAFXMainWindow()
        # mw.writeToMessageArea("onTabChanged") # 被注释掉的调试信息。
        try:
            # 获取当前选中的标签页索引。
            # 获取当前选中的标签页索引
            current_tab = sender.getCurrent()
            # 如果当前标签页索引为 0 (通常是 HTC 相关的标签页)。
            if current_tab==0:
                try:
                    # currentModelName = getCurrentContext().get('modelName', '') # 被注释掉的代码。
                    # x = m.interactions.keys() # 被注释掉的代码。
                    # mw = getAFXApp().getAFXMainWindow() # 被注释掉的代码。
                    # mw.writeToMessageArea(str(x)) # 被注释掉的代码。
                    # 初始化 HTC 列表。
                    self.HTCList = []
                    # 遍历模型中的所有相互作用。
                    for i in m.interactions.keys():
                        # mw.writeToMessageArea(str(m.interactions[i].__name__)) # 被注释掉的调试信息。
                        # 如果相互作用的类型是 'FilmCondition'，则将其名称添加到 HTC 列表中。
                        if m.interactions[i].__name__ == 'FilmCondition':
                            self.HTCList.append(i)
                except Exception as e:
                    # 如果获取模型或相互作用失败，则在消息区域显示“no model”并使用默认 HTC 列表。
                    mw.writeToMessageArea("no model")
                    self.HTCList = ['HTC1', 'HTC2']
                # 如果 HTC 列表不为空。
                if self.HTCList:
                    try:
                        # 遍历 HTC 列表，填充 tableH 表格。
                        for i in range(0, len(self.HTCList)):
                            # 设置表格第一列为 HTC 名称。
                            self.tableH.setItemText(i + 1, 1, str(self.HTCList[i]))
                            # 设置表格第二列为 HTC 相关的占位符。
                            self.tableH.setItemText(i + 1, 2, '%OP%_%NM%_HTC')
                            # 设置表格第三列为温度相关的占位符。
                            self.tableH.setItemText(i + 1, 3, '%OP%_%NM%_TEMP')
                    except Exception as e:
                        # 如果填充表格失败，打印错误信息。
                        print("Error filling table:", str(e))
            # 如果是 Stress 标签页（索引为 1）。
            # 如果是Stress标签页（索引为1）
            elif current_tab == 1:
                try:
                    # 再次获取当前模型。
                    m = self.get_current_model()
                    # 初始化 STRESS 列表。
                    self.STRESSList = []
                    # 获取模型中的所有载荷名称。
                    model_loads = m.loads.keys() 
                    # 如果载荷列表为空。
                    if not model_loads:
                        # mw.writeToMessageArea(u'Debug: 载荷列表为空'.encode('GB18030')) # 被注释掉的调试信息。
                        pass # 不执行任何操作。
                    else:
                        # 否则，将所有载荷名称添加到 STRESS 列表中。
                        for i in model_loads:
                            self.STRESSList.append(i)
                    # mw.writeToMessageArea("Debug: self.STRESSList after try block: " + str(self.STRESSList)) # 被注释掉的调试信息。
                    pass # 不执行任何操作。
                except Exception as e:
                    # 如果获取载荷失败，则在消息区域显示错误信息。
                    mw.writeToMessageArea(str(e))
                # 如果 STRESS 列表不为空。
                if self.STRESSList:
                    try:
                        # 遍历 STRESS 列表，填充 tableL 表格。
                        for i in range(len(self.STRESSList)):
                            # 如果当前行数不足，则插入新行。
                            if i >= self.tableL.getNumRows() - 1: # 如果需要更多行
                                self.tableL.insertRows(self.tableL.getNumRows(), 1) # 在末尾插入新行
                            # 设置表格第一列为载荷名称。
                            self.tableL.setItemText(i + 1, 1, str(self.STRESSList[i]))
                            # 设置表格第二列为载荷相关的占位符。
                            self.tableL.setItemText(i + 1, 2, '%OP%_%NM%')
                    except Exception as e:
                        # 如果填充表格失败，打印错误信息。
                        print("Error filling table:", str(e))
                # 以上为载荷相关的处理。
                # 以上为载荷
                # 以下为预定义场相关的处理。
                # 以下为预定义场
                # 获取文本框 keyword80Kw 的内容。
                # 获取文本框内容
                text = self.form.keyword80Kw.getValue()
                # 如果文本内容不为空。
                if text:
                    # 将内容按逗号分隔解析为列表。
                    # 将内容解析为列表
                    items = text.split(',')
                    # 使用 rename_duplicates 函数处理列表，为重复项添加后缀。
                    # 使用 rename_duplicates 函数修饰列表
                    renamed_items = self.rename_duplicates(items)
                    # 获取表格控件 table1 的引用。
                    # 获取表格控件
                    table = self.getTable1()
                    # 清空表格中除第一行（表头）外的所有内容。
                    # 清空表格内容
                    for row in range(1, table.getNumRows()):  # 从第2行开始
                        for col in range(1, table.getNumColumns()):  # 从第2列开始
                            table.setItemText(row, col, "")
                    # 遍历处理后的列表，将数据填充到表格中。
                    # 填充表格
                    for i, item in enumerate(renamed_items):
                        # 填充第一列。
                        # 第一列
                        table.setItemText(i + 1, 1, item)
                        # 填充第二列。
                        # 第二列
                        # 如果元素是 'HOLDING'，则第二列设置为 'Propagated'。
                        if item == 'HOLDING':
                            table.setItemText(i + 1, 2, 'Propagated')
                        else:
                            # 否则，设置为当前项的索引加 1。
                            table.setItemText(i + 1, 2, str(i + 1))
                        # 填充第三列，固定为 '0'。
                        # 第三列
                        table.setItemText(i + 1, 3, '0')
                        # 填充第四列，设置为当前项的索引加 1。
                        # 第四列
                        table.setItemText(i + 1, 4, str(i + 1))
                        # 填充第五列，固定为 '-1'。
                        # 第五列
                        table.setItemText(i + 1, 5, '-1')
                    # 更新表格显示。
                    # 更新表格显示
                    table.update()
        except Exception as e:
            # 如果在标签页切换处理过程中发生错误，则在消息区域显示错误信息。
            mw.writeToMessageArea("Error in onTabChanged: " + str(e))
    def onCombineCommand(self, sender, sel, ptr):
        # 此方法在执行“组合”命令时被调用。
        def getTableData(table):
            # 嵌套函数：从表格控件中提取数据。
            # 获取表格的行数。
            num_rows = table.getNumRows()
            # 获取表格的列数。
            num_cols = table.getNumColumns()
            # 初始化存储表格数据的列表。
            tabledata = []
            # 遍历表格的每一行（从第二行开始，跳过表头）。
            for row in range(1, num_rows):  
                # 初始化当前行的数据列表。
                row_data = []
                # 遍历表格的每一列（从第二列开始，跳过行标题）。
                for col in range(1, num_cols):  # 列从1到num_cols
                    # 获取当前单元格的值。
                    value = table.getItemText(row, col)
                    # 如果值不为空白，则添加到行数据中。
                    if value.strip():
                        row_data.append(value)
                # 如果行数据不为空，则将其作为元组添加到总数据列表中。
                if row_data:
                    tabledata.append(tuple(row_data))
            # 返回提取出的表格数据。
            return tabledata
        # 根据当前选中的 TabBook_5 标签页来确定要处理的表格和数据。
        if self.TabBook_5.getCurrent()==0:
            # 如果是第一个标签页（索引为 0），通常对应 HTC。
            table=self.tableH
            importedODBName=''
            tableFdata=((),)
        elif self.TabBook_5.getCurrent()==1:
            # 如果是第二个标签页（索引为 1），通常对应 Stress。
            table=self.tableL
            tableF=self.table1
            # 获取 tableF 的数据。
            tableFdata=getTableData(tableF)
            # 获取导入的 ODB 文件名。
            temp_odb = self.importOdbName.getText().strip()
            # 如果 ODB 文件名不为空。
            if temp_odb:
                # 如果文件名不以 '.odb' 结尾，则添加后缀。
                if not temp_odb.endswith('.odb'):
                    temp_odb += '.odb'
                importedODBName = temp_odb
            else:
                # 否则，导入的 ODB 文件名为空。
                importedODBName=''
        # 获取当前选定表格的数据。
        tabledata=getTableData(table)
        # 获取 AFX 应用程序的主窗口。
        mw = getAFXApp().getAFXMainWindow()
        # 在消息区域显示表格数据（用于调试）。
        mw.writeToMessageArea(str(tuple(tabledata)))
        # 调用 kernelCombine 函数，传递所有必要的参数。
        self.callkernelCombine(
            tabledata=tuple(tabledata),
            # 解析 keyword92Kw 的值作为 bstep，如果为空则为 []。
            bstep=self.form.keyword92Kw.getValue().split(',') if self.form.keyword92Kw.getValue() else [],
            # 解析 keyword80Kw 的值作为 csteplist，如果为空则为 []。
            csteplist=self.form.keyword80Kw.getValue().split(',') if self.form.keyword80Kw.getValue() else [],
            # 解析 keyword93Kw 的值作为 astep，如果为空则为 []。
            astep=self.form.keyword93Kw.getValue().split(',') if self.form.keyword93Kw.getValue() else [],
            # 获取 keyword81Kw 的值作为 cyctimes。
            cyctimes=self.form.keyword81Kw.getValue(),
            # 传递 fieldData。
            fieldData=tuple(tableFdata),
            # 传递导入的 ODB 文件名。
            importedODBName=importedODBName,)
        # 返回 1 表示命令执行成功。
        return 1
    def callkernelCombine(self, tabledata, bstep, csteplist, cyctimes, astep,
      fieldData,importedODBName='',lossDefaultStrategy='Propagated',preFieldname='ImportedTemperature'):
        # 此方法构建并发送一个命令字符串到后端内核进行组合操作。
        # 构建命令字符串，使用格式化字符串填充参数。
        cmds=("""
import AmpPair # 导入 AmpPair 模块。
AmpPair.kernelCombine( # 调用 AmpPair 模块中的 kernelCombine 函数。
    tabledata={}, # 表格数据。
    bstep={}, # bstep 参数。
    csteplist={}, # csteplist 参数。
    cyctimes={}, # cyctimes 参数。
    astep={}, # astep 参数。
    lossDefaultStrategy='{}', # 默认损失策略。
    fieldData={}, # 场数据。
    preFieldname='{}', # 预定义场名称。
    importedODBName='{}', # 导入的 ODB 文件名。
    )""".format(tabledata,bstep,csteplist,cyctimes,astep,lossDefaultStrategy,fieldData,preFieldname,importedODBName))
        # 发送构建好的命令字符串。
        sendCommand(cmds)
        # pass 语句，表示此函数体为空，或者暂时不执行任何操作。
        pass


# Class definition # 类定义开始。
###########################################################################

class STPM_test1033DBFileHandler(FXObject):
    # STPM_test1033DBFileHandler 类，继承自 FXObject，用于处理文件选择。

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def __init__(self, form, keyword, patterns='*'):
        # 构造函数，初始化文件处理器。
        # 存储表单对象。
        self.form = form
        # 存储文件模式（如 "*.txt", "*.inp"）。
        self.patterns = patterns
        # 创建一个 AFXIntTarget 实例，用于模式选择。
        self.patternTgt = AFXIntTarget(0)
        # 动态设置 fileNameKw 属性，指向表单中对应的关键字控件。
        exec('self.fileNameKw = form.%sKw' % keyword)
        # 创建一个 AFXBoolKeyword 实例，用于控制只读模式。
        self.readOnlyKw = AFXBoolKeyword(None, 'readOnly', AFXBoolKeyword.TRUE_FALSE)
        # 调用父类 FXObject 的构造函数。
        FXObject.__init__(self)
        # 注册事件处理函数：当 AFXMode.ID_ACTIVATE 命令被触发时，调用 activate 方法。
        FXMAPFUNC(self, SEL_COMMAND, AFXMode.ID_ACTIVATE, STPM_test1033DBFileHandler.activate)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def activate(self, sender, sel, ptr):
        # 此方法在文件选择器被激活时调用。
        # 如果 fileNameKw 当前没有值，则将其设置为插件目录。
        if not self.fileNameKw.getValue():
            self.fileNameKw.setValue(PLUGIN_DIR)
        # 创建一个 AFXFileSelectorDialog 文件选择对话框。
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
            self.fileNameKw, self.readOnlyKw,
            AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        # 设置只读模式的文件模式为 '.odb'。
        fileDb.setReadOnlyPatterns('*.odb')
        # 设置文件选择对话框的初始目录为插件目录。
        fileDb.setDirectory(PLUGIN_DIR)
        # 创建对话框。
        fileDb.create()
        # 显示模态对话框，等待用户操作。
        fileDb.showModal()


class TesttreeDBFileHandler(FXObject):
    # 定义一个整数常量来代表 fileNameKw 改变事件

    def __init__(self, form, db, keyword, patterns='*'):
        self.form = form
        self.db = db
        self.patterns = patterns
        self.patternTgt = AFXIntTarget(0)
        exec('self.JSONNameKw = form.%sKw' % keyword)
        self.readOnlyKw = AFXBoolKeyword(None, 'readOnly', AFXBoolKeyword.TRUE_FALSE)
        FXObject.__init__(self)
        FXMAPFUNC(self, SEL_COMMAND, AFXMode.ID_ACTIVATE, TesttreeDBFileHandler.activate)
        # 监听 self.JSONNameKw 的改变事件
        self.JSONNameKw.setTarget(self)
        # 使用整数常量作为选择器
        self.JSONNameKw.setSelector(db.ID_FILE_NAME_CHANGED)
        # 映射事件到处理方法
        FXMAPFUNC(self, SEL_COMMAND, db.ID_FILE_NAME_CHANGED, TesttreeDBFileHandler.onFileNameChanged)

    def activate(self, sender, sel, ptr):
        if not self.JSONNameKw.getValue():
            self.JSONNameKw.setValue(PLUGIN_DIR)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.JSONNameKw, self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
        fileDb.setDirectory(PLUGIN_DIR)
        fileDb.create()
        fileDb.showModal()

    def onFileNameChanged(self, sender, sel, ptr):
        # 当 self.JSONNameKw 改变时，调用 outputFilePath 方法
        self.outputFilePath(sender, sel, ptr)

    def outputFilePath(self, sender, sel, ptr):
        selectedFilePath = self.JSONNameKw.getValue()
        mw = getAFXApp().getAFXMainWindow()
        mw.writeToMessageArea('Selected file path: ' + str(selectedFilePath))
        with open(selectedFilePath, 'r') as f:
            self.db.materials_data = json.load(f, object_pairs_hook=OrderedDict)
            # materials = ["Material 1", "Material 2", "Material 3"]
        materials = list(self.db.materials_data.keys())
        materials = [str(material) for material in materials]
        self.db.updateMaterialComboBox(materials)
        self.db.onMaterialChanged(sender, sel, ptr)


class XslFileHandler(FXObject):
    def __init__(self, form, db, keyword, patterns='*'):
        self.form = form
        self.db = db
        self.patterns = patterns
        self.patternTgt = AFXIntTarget(0)
        exec('self.fileNameKw = form.%sKw' % keyword)
        self.readOnlyKw = AFXBoolKeyword(None, 'readOnly', AFXBoolKeyword.TRUE_FALSE)
        FXObject.__init__(self)
        FXMAPFUNC(self, SEL_COMMAND, AFXMode.ID_ACTIVATE, XslFileHandler.activate)
        self.fileNameKw.setTarget(self)
        self.fileNameKw.setSelector(db.ID_FILE_NAME_CHANGED_1)
        FXMAPFUNC(self, SEL_COMMAND, db.ID_FILE_NAME_CHANGED_1, XslFileHandler.onFileNameChanged)
        self.db.fileNameKw = self.fileNameKw

    def activate(self, sender, sel, ptr):
        if not self.fileNameKw.getValue():
            self.fileNameKw.setValue(PLUGIN_DIR)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.fileNameKw, self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
        fileDb.setDirectory(PLUGIN_DIR)
        fileDb.create()
        fileDb.showModal()

    def onFileNameChanged(self, sender, sel, ptr):
        self.outputFilePath(sender, sel, ptr)

    def outputFilePath(self, sender, sel, ptr):
        selectedFilePath = self.fileNameKw.getValue()

        mw = getAFXApp().getAFXMainWindow()
        mw.writeToMessageArea('Selected file path: ' + str(selectedFilePath))

        try:
            # 读取 Excel 文件
            workbook = xlrd.open_workbook(selectedFilePath)
            # 获取所有 Sheet 名称
            sheet_names = workbook.sheet_names()
            if len(sheet_names) > 1:
                mw.writeToMessageArea("yes")

            # 更新 ComboBox_14 的下拉框内容
            combo_box = self.db.getComboBox14()
            combo_box.clearItems()  # 清空现有内容
            for sheet_name in sheet_names:
                combo_box.appendItem(text=str(sheet_name))  # 添加 Sheet 名称到下拉框
            sheet = workbook.sheet_by_index(0)  # 假设读取第一个工作表
            sheet1 = workbook.sheet_by_index(1)
            mw.writeToMessageArea(str(sheet1))

            # 获取表格控件
            table = self.db.getTable()

            # 清空表格内容
            for row in range(table.getNumRows()):
                for col in range(table.getNumColumns()):
                    table.setItemText(row, col, "")  # 将每个单元格内容设置为空字符串

            # 设置表头
            table.setLeadingRowLabels(u'参数名\t参数值\t类型\t部件\t特征'.encode('GB18030'))

            # 填充数据
            for row in range(sheet.nrows):
                for col in range(sheet.ncols):
                    value = sheet.cell_value(row, col)
                    table.setItemText(row + 1, col + 1, str(value))  # 从第二行开始填充数据

            # 更新表格显示
            table.update()

        except Exception as e:
            mw.writeToMessageArea("Error reading Excel file: " + str(e))


class InputFileHandler(FXObject):

    def __init__(self, form, db, keyword, patterns='*'):
        self.form = form
        self.db = db
        self.patterns = patterns
        self.patternTgt = AFXIntTarget(0)
        exec('self.InputDataName = form.%sKw' % keyword)
        self.readOnlyKw = AFXBoolKeyword(None, 'readOnly', AFXBoolKeyword.TRUE_FALSE)
        FXObject.__init__(self)
        FXMAPFUNC(self, SEL_COMMAND, AFXMode.ID_ACTIVATE, InputFileHandler.activate)
        # 监听 self.InputDataName 的改变事件
        self.InputDataName.setTarget(self)
        # 使用整数常量作为选择器
        self.InputDataName.setSelector(db.ID_FILE_NAME_CHANGED)
        # 映射事件到处理方法
        FXMAPFUNC(self, SEL_COMMAND, db.ID_FILE_NAME_CHANGED, InputFileHandler.onFileNameChanged)

    def activate(self, sender, sel, ptr):
        if not self.InputDataName.getValue():
            self.InputDataName.setValue(PLUGIN_DIR)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.InputDataName, self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setDirectory(PLUGIN_DIR)
        fileDb.setReadOnlyPatterns('*.odb')
        fileDb.create()
        fileDb.showModal()

    def onFileNameChanged(self, sender, sel, ptr):
        # 当 self.InputDataName 改变时，调用 outputFilePath 方法
        self.outputFilePath(sender, sel, ptr)

    def outputFilePath(self, sender, sel, ptr):
        selectedFilePath = self.InputDataName.getValue()
        mw = getAFXApp().getAFXMainWindow()
        mw.writeToMessageArea('Selected file path: ' + str(selectedFilePath))
        list = self.db.getList3()
        self.db.steptimepair={}
        steptimepair = self.db.steptimepair
        try:
            # 读取 Excel 文件
            workbook = xlrd.open_workbook(selectedFilePath)
            # 获取所有 Sheet 名称
            sheet_names = workbook.sheet_names()

            # 清空 List_3 的内容
            if list:
                list.clearItems()
            
            # 遍历每个sheet，获取第一列最后一行的数值
            for sheet_name in sheet_names:
                sheet = workbook.sheet_by_name(sheet_name)
                if sheet.nrows > 0:  # 确保sheet不为空
                    # 获取第一列最后一行的值
                    last_row_value = sheet.cell_value(sheet.nrows-1, 0)
                    # 尝试将值转换为浮点数
                    try:
                        last_row_value = float(last_row_value)
                    except (ValueError, TypeError):
                        last_row_value = 1.0  # 如果转换失败，设置默认值
                    
                    # 将sheet名和值添加到steptimepair字典
                    steptimepair[sheet_name] = last_row_value
                    mw.writeToMessageArea(u'添加步长: {} = {}'.format(sheet_name, last_row_value).encode('GB18030'))
                
                # 添加到列表显示
                list.appendItem(text=str(sheet_name))
            
            # 添加HOLDING项
            list.appendItem(text='HOLDING')
            steptimepair.setdefault('HOLDING', 20.0*365*24*3600)  # 默认20年    
            steptimepair.setdefault('*', 1.0)  # 添加默认值
            
            # 输出完整的steptimepair字典
            # mw.writeToMessageArea(u'步长字典: {}'.format(steptimepair).encode('GB18030'))

        except Exception as e:
            mw.writeToMessageArea("Error reading Excel file: " + str(e))

class MaterialDataDialog(AFXDialog):
    def __init__(self, owner, title, item):
        AFXDialog.__init__(self, owner, title, 
                          self.OK|self.CANCEL, 
                          opts=DIALOG_NORMAL, w=400, h=400)
        data = self.get_item_data(owner, item)
        self.temp_data = data  # 存储原始数据的副本
        self.modified_data = None  # 存储修改后的数据
        self.owner = owner
        self.item = item    
        # 创建垂直布局框架
        vframe = FXVerticalFrame(self, opts=LAYOUT_FILL_X|LAYOUT_FILL_Y)
        
        # 创建可编辑表格
        self.table = AFXTable(vframe, 20, 2, 200, 6, None, 0, 
                            AFXTABLE_EDITABLE|LAYOUT_FILL_X|LAYOUT_FILL_Y)
        self.table.setPopupOptions(AFXTable.POPUP_ALL)
        
        # 根据数据结构初始化表格
        self._initialize_table(data)
        
        # 绑定确定按钮事件
        # self.acceptCommand = self.onAccept
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED_OK, self.onAccept)

    def _initialize_table(self, data):
        """根据空值/一维列表/二维列表初始化表格"""
        # 清空表格
        self.table.setTableSize(numRows=1, numColumns=1)
        try:
            if not data:  # 空值处理
                # 默认1行1列占位
                self.table.setTableSize(numRows=2, numColumns=1)  # 行数=2（表头+1行），列数=1
                # self.table.setLeadingRowLabels("Value")
                return
            
            if isinstance(data[0], (list, tuple)):  # 二维列表
                # 确定最大列数
                max_columns = max(len(row) for row in data)
                # 设置表格尺寸：行数=数据行数+1（表头），列数=最大列数
                self.table.setTableSize(numRows=len(data), numColumns=max_columns)
                # 生成表头（Column 1, Column 2...）
                # header = "".join(["Column {}\t".format(i+1) for i in range(max_columns)])
                # self.table.setLeadingRowLabels(header)
                # 填充数据（行和列从1开始）
                for row_idx, row in enumerate(data):
                    for col_idx, value in enumerate(row):
                        if col_idx + 1 > max_columns:  # 防止越界
                            break
                        self.table.setItemText(row_idx , col_idx , str(value))
            else:  # 一维列表
                # 设置表格尺寸：行数=数据长度+1（表头），列数=1
                self.table.setTableSize(numRows=len(data), numColumns=1)
                # self.table.setLeadingRowLabels("Value")
                # 填充数据（列索引固定为1）
                for row_idx, value in enumerate(data):
                    self.table.setItemText(row_idx , 0, str(value))
            if col_idx:
                self.resize(w=min((col_idx+1)*100,800),h=min((row_idx+1)*100,500))
            else:
                self.resize(w=400,h=max(400,min((row_idx+1)*100,500)))
        except Exception as e:
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(str(e))

    def onAccept(self, sender, sel, ptr,*args):
        """保存表格数据（严格检查索引范围）"""
        try:
            num_rows = self.table.getNumRows()
            num_cols = self.table.getNumColumns()
            new_data = []
    
            if num_cols == 1:  # 一维列表
                for row in range(0, num_rows):  
                    value = self.table.getItemText(row, 0)
                    if value.strip():
                        new_data.append((self._convert_value(value),))
            else:  # 二维列表
                for row in range(0, num_rows):  
                    row_data = []
                    for col in range(0, num_cols):  # 列从1到num_cols
                        value = self.table.getItemText(row, col)
                        if value.strip():
                            row_data.append(self._convert_value(value))
                    if row_data:
                        new_data.append(row_data)
            
            
            if new_data:
                self.modified_data = new_data
            else:
                self.modified_data = ((None,),)
            self.update_item_data(self.owner,self.item,self.modified_data)
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea(u"保存成功{}".format(self.modified_data).encode('GB18030'))
            # mw.writeToMessageArea("保存成功{}".format(self.owner.materials_data))
            self.hide()
        except Exception as e:
            showAFXErrorDialog(getAFXApp().getAFXMainWindow(), "保存失败: {}".format(str(e)))

    def _convert_value(self, value_str):
        """尝试转换数据类型"""
        try:
            return json.loads(value_str)
        except:
            try:
                return float(value_str)
            except:
                if value_str.lower() == "true":
                    return True
                elif value_str.lower() == "false":
                    return False
                return str(value_str)

    def getModifiedData(self):
        """获取修改后的数据"""
        return self.modified_data
    
    
    def update_item_data(self,owner,item,new_data):
        path = []
        while item is not None:
            path.append(owner.tree.getItemText(item))
            item = item.getParent()
        path.reverse()
        data = owner.materials_data
        #owner.materials_data['2.25Cr1Mo']['Density']['Uniform']['ASME']=new_data
        for level in path[:-1]:
            data = data[level]
            if data is None:
                return None
        data[path[-1]] = new_data
        return data
    
    # 获取item的data
    def get_item_data(self, owner, item):
        path = []
        while item is not None:
            path.append(owner.tree.getItemText(item))
            item = item.getParent()
        path.reverse()
        data = owner.materials_data
        
        # owner.materials_data【path1】【path2】【path3】=
        for level in path:
            data = data.get(level)
            if data is None:
                return None
        return data
