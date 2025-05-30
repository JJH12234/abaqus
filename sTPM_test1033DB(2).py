# -*- coding: utf-8 -*-
from abaqusConstants import *
from abaqusGui import *
from kernelAccess import mdb, session
import os
import json
import xlrd
from collections import OrderedDict

thisPath = os.path.abspath(__file__)
thisDir = os.path.dirname(thisPath)


###########################################################################
# Class definition
###########################################################################

class STPM_test1033DB(AFXDataDialog):
    [
        ID_CLICKED_RUN1,
        ID_MODEL_MATERIAL_COMBO_CHANGED,
        ID_CLICKED_NEW,
        ID_CLICKED,
        ID_COMBO_CHANGED,
        ID_FILE_CHANGED,
        ID_FILE_NAME_CHANGED,
        ID_FILE_NAME_CHANGED_1,
        ID_CLICKED_LIST
    ] = range(AFXForm.ID_LAST, AFXForm.ID_LAST + 9)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def __init__(self, form):

        # Construct the base class.
        
        AFXDataDialog.__init__(self, form, 'STPM_GB_Before',
            self.OK|self.CANCEL, )
        self.materials_data = {}
        self.form = form
        okBtn = self.getActionButton(self.ID_CLICKED_OK)
        okBtn.setText('OK')
             
        TabBook_1 = FXTabBook(p=self, tgt=None, sel=0,
            opts=TABBOOK_NORMAL|LAYOUT_FILL_X|LAYOUT_FILL_Y,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)

        
        # try:
        #     excel_path = u"D:/psh/1学习/工作/寿命评价/ParaModelingData.xls"
        #     workbook = xlrd.open_workbook(excel_path)
        #     sheet = workbook.sheet_by_index(0)  
            
            
        #     self.excel_data = []
        #     for row in range(sheet.nrows):
        #         row_data = []
        #         for col in range(sheet.ncols):
        #             value = sheet.cell_value(row, col)
        #             row_data.append(value)
        #         self.excel_data.append(row_data)
        # except Exception as e:
        #     mw = getAFXApp().getAFXMainWindow()
        #     mw.writeToMessageArea("Error reading Excel file:"+ str(e))
        #     self.excel_data = None

        tabItem = FXTabItem(p=TabBook_1, text='Geometry', ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_22 = FXVerticalFrame(p=TabBook_1,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        fileHandler = XslFileHandler(form, self, 'fileName', '(*.xls)')
        fileTextHf = FXHorizontalFrame(p=TabItem_22, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        fileTextHf.setSelector(99)
        AFXTextField(p=fileTextHf, ncols=60, labelText='File name:', tgt=form.fileNameKw, sel=0,
            opts=AFXTEXTFIELD_STRING|LAYOUT_CENTER_Y)
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL )
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
            opts=BUTTON_NORMAL|LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        self.ComboBox_14 = AFXComboBox(p=TabItem_22, ncols=0, nvis=1, text='Model:', tgt=form.keyword95Kw, sel=0)
        self.ComboBox_14.setMaxVisible(10)
        self.ComboBox_14.appendItem(text='111')
        self.ComboBox_14.appendItem(text='222')
        self.ComboBox_14.appendItem(text='...')
        self.ComboBox_14.setTarget(self)
        self.ComboBox_14.setSelector(self.ID_COMBO_CHANGED)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_COMBO_CHANGED, self.onSheetChanged)

        vf = FXVerticalFrame(TabItem_22, FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X,
            0,0,0,0, 0,0,0,0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        self.table = AFXTable(vf, 20, 5, 200, 6, form.keyword97Kw, 0, AFXTABLE_EDITABLE | LAYOUT_FILL_X)
        self.table.setPopupOptions(
            AFXTable.POPUP_CUT | AFXTable.POPUP_COPY | AFXTable.POPUP_PASTE | AFXTable.POPUP_INSERT_ROW | AFXTable.POPUP_DELETE_ROW | AFXTable.POPUP_CLEAR_CONTENTS | AFXTable.POPUP_READ_FROM_FILE | AFXTable.POPUP_WRITE_TO_FILE)
        self.table.setLeadingRows(1)
        self.table.setLeadingColumns(1)
        self.table.setColumnWidth(1, 200)
        self.table.setColumnType(1, AFXTable.TEXT)
        self.table.setColumnWidth(2, 200)
        self.table.setColumnType(2, AFXTable.TEXT)
        self.table.setColumnWidth(3, 200)
        self.table.setColumnType(3, AFXTable.TEXT)
        self.table.setColumnWidth(4, 200)
        self.table.setColumnType(4, AFXTable.TEXT)
        self.table.setColumnWidth(5, 200)
        self.table.setColumnType(5, AFXTable.TEXT)
        self.table.setLeadingRowLabels('para\tvalue\ttype\tpart\tfeature')
        self.table.setStretchableColumn(self.table.getNumColumns() - 1)
        self.table.showHorizontalGrid(True)
        self.table.showVerticalGrid(True)
            
        # if self.excel_data:
        #     try:
                
        #         for i in range(0, len(self.excel_data)):
        #             for j in range(0, len(self.excel_data[i])):
        #                 table.setItemText(i+1, j+1, str(self.excel_data[i][j]))
        #     except Exception as e:
        #         print("Error filling table:", str(e))

                
        fileName = os.path.join(thisDir, 'icon.png')
        icon = afxCreatePNGIcon(fileName)
        FXLabel(p=TabItem_22, text='', ic=icon)
        fileName = os.path.join(thisDir, 'icon.png')
        icon = afxCreatePNGIcon(fileName)
        FXLabel(p=TabItem_22, text='', ic=icon)
        fileName = os.path.join(thisDir, 'icon.png')
        icon = afxCreatePNGIcon(fileName)
        FXLabel(p=TabItem_22, text='', ic=icon)
        button = FXButton(p=TabItem_22, text='para change', opts=BUTTON_NORMAL|JUSTIFY_LEFT)
        tabItem = FXTabItem(p=TabBook_1, text='Materials', ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_16 = FXVerticalFrame(p=TabBook_1,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        fileHandler = TesttreeDBFileHandler(form, self,'JSONName', 'All files (*)')
        fileTextHf = FXHorizontalFrame(p=TabItem_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        fileTextHf.setSelector(99)
        AFXTextField(p=fileTextHf, ncols=50, labelText='JSON:', tgt=form.JSONNameKw, sel=0,
            opts=AFXTEXTFIELD_STRING|LAYOUT_CENTER_Y)
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL )
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
            opts=BUTTON_NORMAL|LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        HFrame_26 = FXHorizontalFrame(p=TabItem_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VFrame_24 = FXVerticalFrame(p=HFrame_26, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        self.ComboBox_12 = AFXComboBox(p=VFrame_24, ncols=0, nvis=1, text='Material', tgt=form.keyword88Kw, sel=0)
        self.ComboBox_12.setMaxVisible(10)
        self.ComboBox_12.appendItem(text='2.25Cr1Mo')
        self.ComboBox_12.appendItem(text='SA336')
        self.ComboBox_12.appendItem(text='Na')
        self.ComboBox_12.setTarget(self)
        self.ComboBox_12.setSelector(self.ID_COMBO_CHANGED)
        FXMAPFUNC(self, SEL_DOUBLECLICKED, self.ID_CLICKED, STPM_test1033DB.onItemDoubleClicked)

        selected_material = self.ComboBox_12.getItemText(self.ComboBox_12.getCurrentItem())


        self.listVf = FXVerticalFrame(p=VFrame_24, opts=FRAME_SUNKEN|FRAME_THICK, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        self.listVf.setSelector(99)
        self.tree = FXTreeList(self.listVf, 20, tgt=self, sel=self.ID_CLICKED,
                               opts=LAYOUT_FILL_X | LAYOUT_FILL_Y |
                                    TREELIST_SHOWS_BOXES | TREELIST_ROOT_BOXES |
                                    TREELIST_SHOWS_LINES | LAYOUT_FIX_WIDTH |TREELIST_CHECK_BOXES|HSCROLLER_ALWAYS|TREELIST_PROPAGATE_CHECKS,
                               x=0, y=0, w=200, h=0)
        self.updateTree(selected_material)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_COMBO_CHANGED, STPM_test1033DB.onMaterialChanged)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_CLICKED, STPM_test1033DB.onTreeCheckChanged)  # 新增事件绑定
        # List_1 = AFXList(p=listVf, nvis=20, tgt=form.keyword47Kw, sel=0, opts=HSCROLLING_OFF|LIST_SINGLESELECT)
        # List_1.appendItem(text='  -Prime property1')
        # List_1.appendItem(text='    -Secondary propetry1')
        # List_1.appendItem(text='       Data1(ASME)')
        # List_1.appendItem(text='       Data2(Test)')
        # List_1.appendItem(text='    +Secondary propetry2')
        # List_1.appendItem(text='\xa1\xa1')
        # List_1.appendItem(text='  +Prime property2')
        # List_1.appendItem(text='  +Prime property3')
        if isinstance(HFrame_26, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_26, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_26, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        VFrame_14 = FXVerticalFrame(p=HFrame_26, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=100, pb=0)
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_14, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        HFrame_27 = FXHorizontalFrame(p=VFrame_14, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VFrame_25 = FXVerticalFrame(p=HFrame_27, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_25, text='\xa1\xa1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_25, text='>>', opts=JUSTIFY_LEFT)
        if isinstance(HFrame_27, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_27, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_27, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        VFrame_16 = FXVerticalFrame(p=HFrame_27, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        HFrame_38 = FXHorizontalFrame(p=VFrame_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        m = self.get_current_model()
        form.keyword99Kw.setValue(m.name)
        AFXTextField(p=HFrame_38, ncols=12, labelText="ModelName:", tgt=form.keyword99Kw, sel=0)
        self.ComboBox_15 = AFXComboBox(p=HFrame_38, ncols=0, nvis=1, text='Material :', tgt=form.keyword98Kw, sel=self.ID_MODEL_MATERIAL_COMBO_CHANGED)
        self.ComboBox_15.setTarget(self)
        self.ComboBox_15.setSelector(self.ID_MODEL_MATERIAL_COMBO_CHANGED)
        FXMAPFUNC(self, SEL_COMMAND, self.ID_MODEL_MATERIAL_COMBO_CHANGED, STPM_test1033DB.model_meterial_changed)
        self.ComboBox_15.setMaxVisible(10)
        for material in m.materials.keys():
            self.ComboBox_15.appendItem(text=material)
        self.ComboBox_15.appendItem(text='New')
        self.newMaterialText = AFXTextField(p=HFrame_38, ncols=12, labelText='New:', 
            tgt=form.keyword100Kw, sel=0, opts=0)
        self.newMaterialText.hide()
        # AFXTextField(p=HFrame_38, ncols=12, labelText='New:', tgt=form.keyword100Kw, sel=0)
        HFrame_31 = FXHorizontalFrame(p=VFrame_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VAligner_9 = AFXVerticalAligner(p=HFrame_31, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        spinner = AFXSpinner(VAligner_9, 6, 'UVARM vars', form.keyword61Kw, 0)
        spinner.setRange(1, 10)
        spinner.setIncrement(1)
        spinner = AFXSpinner(VAligner_9, 6, 'SDV vars', form.keyword62Kw, 0)
        spinner.setRange(1, 10)
        spinner.setIncrement(1)
        run1 = FXButton(p=VFrame_16, text='run1', ic=None, tgt=self,
                                     sel=self.ID_CLICKED_RUN1,
                                     opts=BUTTON_NORMAL |LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        #l = FXLabel(p=VFrame_16, text='Button: Run', opts=JUSTIFY_LEFT)
        tabItem = FXTabItem(p=TabBook_1, text='Amplitudes', ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_6 = FXVerticalFrame(p=TabBook_1,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        GroupBox_5 = FXGroupBox(p=TabItem_6, text='Data Format', opts=FRAME_GROOVE|LAYOUT_FILL_X)
        HFrame_8 = FXHorizontalFrame(p=GroupBox_5, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        GroupBox_2 = FXGroupBox(p=HFrame_8, text='Separated XY*', opts=FRAME_GROOVE|LAYOUT_FILL_X)
        HFrame_6 = FXHorizontalFrame(p=GroupBox_2, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        FXRadioButton(p=HFrame_6, text='Separated', tgt=form.DataXYTypeKw1, sel=53)
        FXRadioButton(p=HFrame_6, text='Unified X', tgt=form.DataXYTypeKw1, sel=54)
        GroupBox_3 = FXGroupBox(p=HFrame_8, text='Based On...', opts=FRAME_GROOVE|LAYOUT_FILL_X)
        HFrame_7 = FXHorizontalFrame(p=GroupBox_3, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        FXRadioButton(p=HFrame_7, text='Step Time', tgt=form.DataTimeTypeKw1, sel=55)
        if isinstance(HFrame_7, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_7, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_7, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        FXRadioButton(p=HFrame_7, text='Time(h)', tgt=form.TimeUnitKw1, sel=56)
        FXRadioButton(p=HFrame_7, text='Time(s)', tgt=form.TimeUnitKw1, sel=57)
        fileHandler = InputFileHandler(form, self, 'InputDataName', '(*.xls)')
        fileTextHf = FXHorizontalFrame(p=TabItem_6, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        fileTextHf.setSelector(99)
        AFXTextField(p=fileTextHf, ncols=70, labelText='File name:', tgt=form.InputDataNameKw, sel=0,
            opts=AFXTEXTFIELD_STRING|LAYOUT_CENTER_Y)
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL )
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
            opts=BUTTON_NORMAL|LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        if isinstance(TabItem_6, FXHorizontalFrame):
            FXVerticalSeparator(p=TabItem_6, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=TabItem_6, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        run2 = FXButton(p=TabItem_6, text='run2', ic=None, tgt=fileHandler,
                                     sel=AFXMode.ID_ACTIVATE + 1,
                                     opts=BUTTON_NORMAL, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        #l = FXLabel(p=TabItem_6, text='Button: Run', opts=JUSTIFY_LEFT)
        l.setFont( getAFXFont(FONT_BOLD) )
        HFrame_10 = FXHorizontalFrame(p=TabItem_6, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        GroupBox_7 = FXGroupBox(p=HFrame_10, text='*Separated Example', opts=FRAME_GROOVE)
        HFrame_9 = FXHorizontalFrame(p=GroupBox_7, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VFrame_1 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_1, text='Time1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_1, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_1, text='1', opts=JUSTIFY_LEFT)
        VFrame_2 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_2, text='Amp1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_2, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_2, text='100', opts=JUSTIFY_LEFT)
        if isinstance(HFrame_9, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_9, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_9, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        VFrame_3 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_3, text='Time2', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_3, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_3, text='0.5', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_3, text='1', opts=JUSTIFY_LEFT)
        VFrame_4 = FXVerticalFrame(p=HFrame_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_4, text='Amp2', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_4, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_4, text='100', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_4, text='500', opts=JUSTIFY_LEFT)
        if isinstance(HFrame_10, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_10, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_10, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        GroupBox_8 = FXGroupBox(p=HFrame_10, text='*Unified Example', opts=FRAME_GROOVE)
        HFrame_11 = FXHorizontalFrame(p=GroupBox_8, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VFrame_7 = FXVerticalFrame(p=HFrame_11, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_7, text='Time', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_7, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_7, text='0.5', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_7, text='1', opts=JUSTIFY_LEFT)
        if isinstance(HFrame_11, FXHorizontalFrame):
            FXVerticalSeparator(p=HFrame_11, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=HFrame_11, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        VFrame_6 = FXVerticalFrame(p=HFrame_11, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_6, text='Amp1', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_6, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_6, text='50', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_6, text='100', opts=JUSTIFY_LEFT)
        VFrame_5 = FXVerticalFrame(p=HFrame_11, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        l = FXLabel(p=VFrame_5, text='Amp2', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_5, text='0', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_5, text='100', opts=JUSTIFY_LEFT)
        l = FXLabel(p=VFrame_5, text='500', opts=JUSTIFY_LEFT)
        tabItem = FXTabItem(p=TabBook_1, text='Cycle', ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_5 = FXVerticalFrame(p=TabBook_1,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        HFrame_32 = FXHorizontalFrame(p=TabItem_5, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        listVf = FXVerticalFrame(p=HFrame_32, opts=FRAME_SUNKEN|FRAME_THICK, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        listVf.setSelector(99)
        self.List_3 = AFXList(p=listVf, nvis=10, tgt=form.keyword64Kw, sel=0, opts=HSCROLLING_OFF | LIST_SINGLESELECT)
        self.List_3.appendItem(text='1')
        self.List_3.appendItem(text='2')
        self.List_3.appendItem(text='3')
        self.List_3.appendItem(text='4')
        self.List_3.appendItem(text='5')

        self.List_3.setTarget(self)
        self.List_3.setSelector(self.ID_CLICKED_LIST)
        FXMAPFUNC(self, SEL_DOUBLECLICKED, self.ID_CLICKED_LIST, STPM_test1033DB.onListItemDoubleClicked)

        VFrame_17 = FXVerticalFrame(p=HFrame_32, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        VAligner_10 = AFXVerticalAligner(p=VFrame_17, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        AFXTextField(p=VAligner_10, ncols=50, labelText='     before:', tgt=form.keyword68Kw, sel=0)
        AFXTextField(p=VAligner_10, ncols=50, labelText='>>Cycle List:', tgt=form.keyword65Kw, sel=0)
        AFXTextField(p=VAligner_10, ncols=50, labelText='     after:', tgt=form.keyword69Kw, sel=0)
        AFXTextField(p=VAligner_10, ncols=12, labelText='     Cycle times:', tgt=form.keyword77Kw, sel=0)
        AFXTextField(p=VAligner_10, ncols=12, labelText='     HOLDING time:', tgt=form.keyword94Kw, sel=0)
        HFrame_33 = FXHorizontalFrame(p=VFrame_17, opts=0, x=0, y=0, w=0, h=0,
            pl=20, pr=0, pt=0, pb=0)
        FXRadioButton(p=HFrame_33, text='NEW', tgt=form.HFrame33Kw1, sel=58)
        FXRadioButton(p=HFrame_33, text='REPLACE', tgt=form.HFrame33Kw1, sel=59)
        ComboBox_13 = AFXComboBox(p=HFrame_33, ncols=0, nvis=1, text='       StepType:', tgt=form.keyword90Kw, sel=0)
        ComboBox_13.setMaxVisible(10)
        ComboBox_13.appendItem(text='AUTO')
        ComboBox_13.appendItem(text='StaticStep')
        ComboBox_13.appendItem(text='ViscoStep')
        ComboBox_13.appendItem(text='HeatTransferStep')
        createstep = FXButton(p=VFrame_17, text='creat step', ic=None, tgt=fileHandler,
                                     sel=AFXMode.ID_ACTIVATE + 1,
                                     opts=BUTTON_NORMAL | LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        l = FXLabel(p=VFrame_17, text='Button: creat step', opts=JUSTIFY_LEFT)
        if isinstance(TabItem_5, FXHorizontalFrame):
            FXVerticalSeparator(p=TabItem_5, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        else:
            FXHorizontalSeparator(p=TabItem_5, x=0, y=0, w=0, h=0, pl=2, pr=2, pt=2, pb=2)
        GroupBox_23 = FXGroupBox(p=TabItem_5, text='Quickly setting tools', opts=FRAME_GROOVE)
        HFrame_34 = FXHorizontalFrame(p=GroupBox_23, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        FXRadioButton(p=HFrame_34, text='\xa1\xa1', tgt=form.GroupBox23Kw1, sel=60)
        AFXTextField(p=HFrame_34, ncols=12, labelText='for step name contains:', tgt=form.keyword72Kw, sel=0)
        FXRadioButton(p=HFrame_34, text='or', tgt=form.GroupBox23Kw1, sel=61)
        AFXTextField(p=HFrame_34, ncols=12, labelText='for N*n+i step; n,i=', tgt=form.keyword73Kw, sel=0)
        HFrame_35 = FXHorizontalFrame(p=GroupBox_23, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        ComboBox_11 = AFXComboBox(p=HFrame_35, ncols=0, nvis=1, text='method:', tgt=form.keyword70Kw, sel=0)
        ComboBox_11.setMaxVisible(10)
        ComboBox_11.appendItem(text='set Time period')
        ComboBox_11.appendItem(text='set Initial increment size')
        ComboBox_11.appendItem(text='set Max increment size')
        ComboBox_11.appendItem(text='set Ramp linearly over step')
        ComboBox_11.appendItem(text='set Instantaneous')
        ComboBox_11.appendItem(text='change Static to Visco')
        ComboBox_11.appendItem(text='change Visco to Static')
        ComboBox_11.appendItem(text='enable Restart')
        AFXTextField(p=HFrame_35, ncols=12, labelText=' Value:', tgt=form.keyword74Kw, sel=0)
        l = FXLabel(p=GroupBox_23, text='Button: modify step', opts=JUSTIFY_LEFT)
        tabItem = FXTabItem(p=TabBook_1, text='Loads&&HTC', ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_7 = FXVerticalFrame(p=TabBook_1,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        GroupBox_26 = FXGroupBox(p=TabItem_7, text='Step List (%OP%)', opts=FRAME_GROOVE)
        VAligner_11 = AFXVerticalAligner(p=GroupBox_26, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        AFXTextField(p=VAligner_11, ncols=12, labelText='before:', tgt=form.keyword92Kw, sel=0)
        AFXTextField(p=VAligner_11, ncols=50, labelText='Composition of cycle:', tgt=form.keyword80Kw, sel=0)
        AFXTextField(p=VAligner_11, ncols=12, labelText='after:', tgt=form.keyword93Kw, sel=0)
        AFXTextField(p=VAligner_11, ncols=12, labelText='Cycle times:', tgt=form.keyword81Kw, sel=0)
        TabBook_5 = FXTabBook(p=TabItem_7, tgt=None, sel=0,
            opts=TABBOOK_NORMAL,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)
        tabItem = FXTabItem(p=TabBook_5, text='Heat', ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_20 = FXVerticalFrame(p=TabBook_5,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        vf = FXVerticalFrame(TabItem_20, FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X,
            0,0,0,0, 0,0,0,0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        table = AFXTable(vf, 6, 4, 6, 4, form.keyword78Kw, 0, AFXTABLE_EDITABLE|LAYOUT_FILL_X)
        table.setPopupOptions(AFXTable.POPUP_CUT|AFXTable.POPUP_COPY|AFXTable.POPUP_PASTE|AFXTable.POPUP_INSERT_ROW|AFXTable.POPUP_DELETE_ROW|AFXTable.POPUP_CLEAR_CONTENTS|AFXTable.POPUP_READ_FROM_FILE|AFXTable.POPUP_WRITE_TO_FILE)
        table.setLeadingRows(1)
        table.setLeadingColumns(1)
        table.setColumnWidth(1, 200)
        table.setColumnType(1, AFXTable.TEXT)
        table.setColumnWidth(2, 200)
        table.setColumnType(2, AFXTable.TEXT)
        table.setColumnWidth(3, 200)
        table.setColumnType(3, AFXTable.TEXT)
        table.setLeadingRowLabels('HTC (%NM%)\tAmpName\tTempName')
        table.setStretchableColumn( table.getNumColumns()-1 )
        table.showHorizontalGrid(True)
        table.showVerticalGrid(True)
        tabItem = FXTabItem(p=TabBook_5, text='Stress', ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_21 = FXVerticalFrame(p=TabBook_5,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        vf = FXVerticalFrame(TabItem_21, FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X,
            0,0,0,0, 0,0,0,0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        table = AFXTable(vf, 6, 3, 6, 3, form.keyword82Kw, 0, AFXTABLE_EDITABLE|LAYOUT_FILL_X)
        table.setPopupOptions(AFXTable.POPUP_CUT|AFXTable.POPUP_COPY|AFXTable.POPUP_PASTE|AFXTable.POPUP_INSERT_ROW|AFXTable.POPUP_DELETE_ROW|AFXTable.POPUP_CLEAR_CONTENTS|AFXTable.POPUP_READ_FROM_FILE|AFXTable.POPUP_WRITE_TO_FILE)
        table.setLeadingRows(1)
        table.setLeadingColumns(1)
        table.setColumnWidth(1, 200)
        table.setColumnType(1, AFXTable.FLOAT)
        table.setColumnWidth(2, 100)
        table.setColumnType(2, AFXTable.FLOAT)
        table.setLeadingRowLabels('Load (%NM%)\tAmp')
        table.setStretchableColumn( table.getNumColumns()-1 )
        table.showHorizontalGrid(True)
        table.showVerticalGrid(True)
        vf = FXVerticalFrame(TabItem_21, FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X,
            0,0,0,0, 0,0,0,0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        table = AFXTable(vf, 12, 6, 12, 6, form.keyword83Kw, 0, AFXTABLE_EDITABLE|LAYOUT_FILL_X)
        table.setLeadingRows(1)
        table.setLeadingColumns(1)
        table.setColumnWidth(1, 200)
        table.setColumnType(1, AFXTable.FLOAT)
        table.setColumnWidth(2, 100)
        table.setColumnType(2, AFXTable.FLOAT)
        table.setColumnWidth(3, 100)
        table.setColumnType(3, AFXTable.FLOAT)
        table.setColumnWidth(4, 100)
        table.setColumnType(4, AFXTable.FLOAT)
        table.setColumnWidth(5, 100)
        table.setColumnType(5, AFXTable.FLOAT)
        table.setLeadingRowLabels('Temperature field at OP\tstart Step\tstart Inc\tend Step\tend Inc')
        table.setStretchableColumn( table.getNumColumns()-1 )
        table.showHorizontalGrid(True)
        table.showVerticalGrid(True)
        tabItem = FXTabItem(p=TabBook_1, text='Fortran Subroutine', ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_9 = FXVerticalFrame(p=TabBook_1,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        fileHandler = STPM_test1033DBFileHandler(form, 'SubroutineName', '*.for')
        fileTextHf = FXHorizontalFrame(p=TabItem_9, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        fileTextHf.setSelector(99)
        AFXTextField(p=fileTextHf, ncols=70, labelText='SubroutineName:', tgt=form.SubroutineNameKw, sel=0,
            opts=AFXTEXTFIELD_STRING|LAYOUT_CENTER_Y)
        icon = afxGetIcon('fileOpen', AFX_ICON_SMALL )
        FXButton(p=fileTextHf, text='	Select File\nFrom Dialog', ic=icon, tgt=fileHandler, sel=AFXMode.ID_ACTIVATE,
            opts=BUTTON_NORMAL|LAYOUT_CENTER_Y, x=0, y=0, w=0, h=0, pl=1, pr=1, pt=1, pb=1)
        GroupBox_14 = FXGroupBox(p=TabItem_9, text='Redefine Creep', opts=FRAME_GROOVE|LAYOUT_FILL_X)
        ComboBox_9 = AFXComboBox(p=GroupBox_14, ncols=0, nvis=1, text='Creep Consti:', tgt=form.keyword43Kw, sel=0)
        ComboBox_9.setMaxVisible(10)
        ComboBox_9.appendItem(text='RCC')
        ComboBox_9.appendItem(text='NB')
        vf = FXVerticalFrame(GroupBox_14, FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X,
            0,0,0,0, 0,0,0,0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        table = AFXTable(vf, 6, 8, 6, 8, form.keyword16Kw, 0, AFXTABLE_EDITABLE|LAYOUT_FILL_X)
        table.setPopupOptions(AFXTable.POPUP_CUT|AFXTable.POPUP_COPY|AFXTable.POPUP_PASTE|AFXTable.POPUP_INSERT_ROW|AFXTable.POPUP_DELETE_ROW|AFXTable.POPUP_CLEAR_CONTENTS|AFXTable.POPUP_READ_FROM_FILE|AFXTable.POPUP_WRITE_TO_FILE)
        table.setLeadingRows(1)
        table.setLeadingColumns(1)
        table.setColumnWidth(1, 100)
        table.setColumnType(1, AFXTable.FLOAT)
        table.setColumnWidth(2, 100)
        table.setColumnType(2, AFXTable.FLOAT)
        table.setColumnWidth(3, 100)
        table.setColumnType(3, AFXTable.FLOAT)
        table.setColumnWidth(4, 100)
        table.setColumnType(4, AFXTable.FLOAT)
        table.setColumnWidth(5, 100)
        table.setColumnType(5, AFXTable.FLOAT)
        table.setColumnWidth(6, 100)
        table.setColumnType(6, AFXTable.FLOAT)
        table.setColumnWidth(7, 100)
        table.setColumnType(7, AFXTable.FLOAT)
        table.setLeadingRowLabels('Temp\tp1\tp2\tp3\tp4\tp5\tp6')
        table.setStretchableColumn( table.getNumColumns()-1 )
        table.showHorizontalGrid(True)
        table.showVerticalGrid(True)
        HFrame_25 = FXHorizontalFrame(p=TabItem_9, opts=LAYOUT_FILL_X, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        GroupBox_12 = FXGroupBox(p=HFrame_25, text='Sr', opts=FRAME_GROOVE|LAYOUT_FILL_Y)
        TabBook_3 = FXTabBook(p=GroupBox_12, tgt=None, sel=0,
            opts=TABBOOK_NORMAL,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)
        tabItem = FXTabItem(p=TabBook_3, text='Name', ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_14 = FXVerticalFrame(p=TabBook_3,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        vf = FXVerticalFrame(TabItem_14, FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X,
            0,0,0,0, 0,0,0,0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        table = AFXTable(vf, 2, 5, 2, 5, form.keyword35Kw, 0, AFXTABLE_EDITABLE|LAYOUT_FILL_X)
        table.setPopupOptions(AFXTable.POPUP_CUT|AFXTable.POPUP_COPY|AFXTable.POPUP_PASTE|AFXTable.POPUP_INSERT_ROW|AFXTable.POPUP_DELETE_ROW|AFXTable.POPUP_CLEAR_CONTENTS|AFXTable.POPUP_READ_FROM_FILE|AFXTable.POPUP_WRITE_TO_FILE)
        table.setLeadingRows(1)
        table.setLeadingColumns(1)
        table.setColumnWidth(1, 50)
        table.setColumnType(1, AFXTable.FLOAT)
        table.setColumnWidth(2, 50)
        table.setColumnType(2, AFXTable.FLOAT)
        table.setColumnWidth(3, 50)
        table.setColumnType(3, AFXTable.FLOAT)
        table.setColumnWidth(4, 50)
        table.setColumnType(4, AFXTable.FLOAT)
        table.setLeadingRowLabels('a\tb\tc\tC')
        table.setStretchableColumn( table.getNumColumns()-1 )
        table.showHorizontalGrid(True)
        table.showVerticalGrid(True)
        GroupBox_22 = FXGroupBox(p=HFrame_25, text='SN', opts=FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y)
        TabBook_4 = FXTabBook(p=GroupBox_22, tgt=None, sel=0,
            opts=TABBOOK_NORMAL,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING)
        tabItem = FXTabItem(p=TabBook_4, text='Name', ic=None, opts=TAB_TOP_NORMAL,
            x=0, y=0, w=0, h=0, pl=6, pr=6, pt=DEFAULT_PAD, pb=DEFAULT_PAD)
        TabItem_18 = FXVerticalFrame(p=TabBook_4,
            opts=FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,
            x=0, y=0, w=0, h=0, pl=DEFAULT_SPACING, pr=DEFAULT_SPACING,
            pt=DEFAULT_SPACING, pb=DEFAULT_SPACING, hs=DEFAULT_SPACING, vs=DEFAULT_SPACING)
        vf = FXVerticalFrame(TabItem_18, FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X,
            0,0,0,0, 0,0,0,0)
        # Note: Set the selector to indicate that this widget should not be
        #       colored differently from its parent when the 'Color layout managers'
        #       button is checked in the RSG Dialog Builder dialog.
        vf.setSelector(99)
        table = AFXTable(vf, 6, 5, 6, 5, form.keyword41Kw, 0, AFXTABLE_EDITABLE|LAYOUT_FILL_X)
        table.setPopupOptions(AFXTable.POPUP_CUT|AFXTable.POPUP_COPY|AFXTable.POPUP_PASTE|AFXTable.POPUP_INSERT_ROW|AFXTable.POPUP_DELETE_ROW|AFXTable.POPUP_CLEAR_CONTENTS|AFXTable.POPUP_READ_FROM_FILE|AFXTable.POPUP_WRITE_TO_FILE)
        table.setLeadingRows(1)
        table.setLeadingColumns(1)
        table.setColumnWidth(1, 50)
        table.setColumnType(1, AFXTable.FLOAT)
        table.setColumnWidth(2, 50)
        table.setColumnType(2, AFXTable.FLOAT)
        table.setColumnWidth(3, 50)
        table.setColumnType(3, AFXTable.FLOAT)
        table.setColumnWidth(4, 50)
        table.setColumnType(4, AFXTable.FLOAT)
        table.setLeadingRowLabels('Life\tcol1\tcol2\tcol3')
        table.setStretchableColumn( table.getNumColumns()-1 )
        table.showHorizontalGrid(True)
        table.showVerticalGrid(True)
        GroupBox_13 = FXGroupBox(p=TabItem_9, text='Redefine KeyType', opts=FRAME_GROOVE|LAYOUT_FILL_X)
        HFrame_12 = FXHorizontalFrame(p=GroupBox_13, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        spinner = AFXSpinner(HFrame_12, 6, 'SkipSteps:', form.keyword08Kw, 0)
        spinner.setRange(1, 999)
        spinner.setIncrement(1)
        spinner = AFXSpinner(HFrame_12, 6, 'Every n Step is a Cycle', form.keyword07Kw, 0)
        spinner.setRange(1, 999)
        spinner.setIncrement(1)
        AFXTextField(p=HFrame_12, ncols=12, labelText='DownStepLength:', tgt=form.keyword30Kw, sel=0)

    def get_current_model(self):    
        viewport = session.currentViewportName
        modelname=session.sessionState[viewport]['modelName']
        return mdb.models[modelname]
    
    def model_meterial_changed(self,form,sel, ptr):
        selected_material = self.ComboBox_15.getItemText(self.ComboBox_15.getCurrentItem())
        # 根据选择显示/隐藏输入框
        if selected_material == "New":
            self.newMaterialText.show()
            self.newMaterialText.getParent().recalc()  # 添加父容器重新布局
        else:
            self.newMaterialText.hide()
            self.newMaterialText.getParent().recalc()  # 添加父容器重新布局
            
        
    def New_model_meterial_DetailDialog(self):
        # 在原有控件创建处修改（HFrame_38父容器中）
        HFrame_38 = FXHorizontalFrame(p=VFrame_16, opts=0, x=0, y=0, w=0, h=0,
            pl=0, pr=0, pt=0, pb=0)
        
        # 原控件创建代码...
        #ComboBox_15 = AFXComboBox(p=HFrame_38, ncols=0, nvis=1, text='Material :', tgt=form.keyword98Kw, sel=0)
        #ComboBox_15.setMaxVisible(10)
        
        # 新增动态显示的新材料输入框（初始隐藏）
        
        
    def updateTree(self, material):
        self.tree.clearItems()

        material_data = self.materials_data.get(material, {})

        def add_subtree(parent, node_name, node_data):
            node_name = str(node_name)
            if isinstance(node_data, dict):
                sub_parent = self.tree.addItemLast(parent, node_name)
                for key, value in node_data.items():
                    add_subtree(sub_parent, key, value)
            else:
                self.tree.addItemLast(parent, node_name)

            self.tree.expandTree(parent)

        root = self.tree.addItemLast(None, material)
        for key, value in material_data.items():
            add_subtree(root, key, value)
        # mw = getAFXApp().getAFXMainWindow()
        # mw.writeToMessageArea(str(material_data))
        self.tree.update()
    
    # 新增复选框状态变化处理函数
    def onTreeCheckChanged(self, sender, sel, ptr):
        item = self.tree.getCurrentItem()
        # 添加显式状态变量和参数校验
        if item and self.tree.getItemCheck(item) == 1:  
            selected_material = self.ComboBox_12.getItemText(self.ComboBox_12.getCurrentItem())
            
            if str(item) == selected_material:
                # 如果是根节点被选中
                # 存储已处理的父节点
                processed_parents = set()
                
                # 获取根节点的第一个子节点（父节点）
                parent_node = item.getFirst()
                
                # 遍历所有父节点
                while parent_node:
                    # 标记当前父节点为已处理
                    parent_name = self.tree.getItemText(parent_node)
                    processed_parents.add(parent_name)
                    
                    # 获取该父节点的第一个子节点（叶子节点）
                    first_leaf = parent_node.getFirst()
                    
                    # 如果该父节点有子节点
                    if first_leaf:
                        # 选中第一个叶子节点
                        self.tree.setItemCheck(first_leaf, True, notify=False)
                        
                        # 取消选中该父节点下的其他叶子节点
                        sibling = first_leaf.getNext()
                        while sibling:
                            self.tree.setItemCheck(sibling, False, notify=False)
                            sibling = sibling.getNext()
                    
                    # 移动到下一个父节点
                    parent_node = parent_node.getNext()
                
                # 更新树的显示
                self.tree.update()
            else:
                if self.tree.isItemLeaf(item):
                    parent = item.getParent()
                    if parent:
                        sibling = parent.getFirst()
                        while sibling:
                            if sibling != item and self.tree.isItemLeaf(sibling):
                                self.tree.setItemCheck(sibling, False, notify=False)  #
                            sibling = sibling.getNext()
                    self.tree.setItemCheck(item, True, notify=False)
                    self.tree.update()
                else:
                    while not self.tree.isItemLeaf(item):
                        child = item.getFirst()
                        self.tree.setItemCheck(child, True, notify=False)
                        sibling = child.getNext()
                        while sibling:
                            self.tree.setItemCheck(sibling, False, notify=False)  #
                            sibling = sibling.getNext()
                        item=child
                    self.tree.setItemCheck(item, True, notify=False)
                    self.tree.update()
         
    def createCheckBox(self, parent, text):
        hFrame = FXHorizontalFrame(self.tree, LAYOUT_FILL_X)
        FXCheckButton(hFrame, text, None, 0, ICON_BEFORE_TEXT)
        self.tree.addItemLast(parent, '')

    def createRadioButton(self, parent, text):
        hFrame = FXHorizontalFrame(self.tree, LAYOUT_FILL_X)
        FXRadioButton(hFrame, text, None, 0, ICON_BEFORE_TEXT)
        self.tree.addItemLast(parent, '')

    def onMaterialChanged(self, sender, sel, ptr):
        selected_material = self.ComboBox_12.getItemText(self.ComboBox_12.getCurrentItem())
        self.updateTree(selected_material)

    def updateMaterialComboBox(self, materials):
        self.ComboBox_12.clearItems()
        for material in materials:
            self.ComboBox_12.appendItem(text=material)

    def onItemDoubleClicked(self, sender, sel, ptr):
        def get_item_data(item):
            path = []
            while item is not None:
                path.append(self.tree.getItemText(item))
                item = item.getParent()
            path.reverse()
            data = self.materials_data
            for level in path:
                data = data.get(level)
                if data is None:
                    return None
            return data
        
        # mw = getAFXApp().getAFXMainWindow()
        # mw.writeToMessageArea('TreeItemDoubleClicked1')
        selected_item = self.tree.getCurrentItem()
        if selected_item:
            node_name = self.tree.getItemText(selected_item)
            selected_item_data = get_item_data(selected_item)
            if isinstance(selected_item_data, list):

                self.showDetailDialog(node_name, selected_item_data)
            # showAFXInformationDialog(mw, str(get_item_path(selected_item)))
        
    def showDetailDialog(self, node_name, material_data):
        detail_dialog = AFXDialog(self, "Details for " + node_name, opts=DIALOG_NORMAL, w=0,
                                    h=0)
        vframe = FXVerticalFrame(detail_dialog, opts=LAYOUT_FILL_X | LAYOUT_FILL_Y)
        # for key, value in material_data.items():
        #     label = FXLabel(vframe, "{}: {}".format(key, str(value)))
        label = FXLabel(vframe, "{}: {}".format(node_name, str(material_data)))
        detail_dialog.create()
        detail_dialog.show()    
    
    
    def show(self):#覆写父类功能

        AFXDataDialog.show(self)

        # Register a query on materials
        #
        session.registerQuery(self.onSessionChange, False)#注册函数到session变化时
        currentModelName = getCurrentContext().get('modelName', '')
        if currentModelName in mdb.models:
            self.regModel=mdb.models[currentModelName]
            self.regModel.materials.registerQuery(self.updateComboBox_15Materials,False)#启动时，先注册当前模型.材料
        self.updateComboBox_15Materials()#执行一次更新
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def hide(self):
        AFXDataDialog.hide(self)
        session.unregisterQuery(self.onSessionChange)#注销是必要的，不然在被监听对象不存在时必定报错
        try:
            self.regModel.materials.unregisterQuery(self.updateComboBox_15Materials)#注销当前模型.材料
        except:
            pass
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def updateComboBox_15Materials(self):
        currentModelName = getCurrentContext().get('modelName', '')
        if currentModelName in mdb.models:
            # 更新ComboBox内容
            self.ComboBox_15.clearItems()
            model = mdb.models[currentModelName]
            names = model.materials.keys()
            if names:
                names.sort()
                for name in names:
                    self.ComboBox_15.appendItem(name)
                self.ComboBox_15.appendItem("New")
            # 更新关键字值
                self.form.keyword98Kw.setValue(names[0] if self.form.keyword98Kw.getValue() not in names else self.form.keyword98Kw.getValue())
            else:
                self.form.keyword98Kw.setValue('')
            self.resize(self.getDefaultWidth(), self.getDefaultHeight())
            return 1
        else:
            return 0
    def onSessionChange(self):
        currentModelName = getCurrentContext().get('modelName', '')
        if currentModelName in mdb.models:
            if self.regModel and getattr(self.regModel,'name',None) != currentModelName: #当模型切换过
                #取消注册旧模型材料
                try:
                    self.regModel.materials.unregisterQuery(self.updateComboBox_15Materials)
                except:
                    pass
            #注册新模型材料
            self.regModel=mdb.models[currentModelName]
            self.regModel.materials.registerQuery(self.updateComboBox_15Materials,False)
            self.updateComboBox_15Materials()#调用一次
            return 1
        else:
            return 0
    def onSheetChanged(self, sender, sel, ptr, *args):
        # 获取当前选中的 Sheet 索引
        selected_sheet_index = self.ComboBox_14.getCurrentItem()

        # 获取文件路径
        selected_file_path = self.fileNameKw.getValue()

        if selected_file_path:
            try:
                # 打开 Excel 文件
                workbook = xlrd.open_workbook(selected_file_path)

                # 获取选中的 Sheet
                sheet = workbook.sheet_by_index(selected_sheet_index)

                # 填充表格数据
                self.fillTableWithSheetData(sheet)
            except Exception as e:
                mw = getAFXApp().getAFXMainWindow()
                mw.writeToMessageArea("Error reading selected sheet: " + str(e))

    def fillTableWithSheetData(self, sheet):
        # 获取表格控件
        table = self.getTable()

        # 清空表格内容
        for row in range(table.getNumRows()):
            for col in range(table.getNumColumns()):
                table.setItemText(row, col, "")  # 将每个单元格内容设置为空字符串

        # 设置表头
        table.setLeadingRowLabels('para\tvalue\ttype\tpart\tfeature')

        # 填充数据
        for row in range(sheet.nrows):
            for col in range(sheet.ncols):
                value = sheet.cell_value(row, col)
                table.setItemText(row + 1, col + 1, str(value))  # 从第二行开始填充数据

        # 更新表格显示
        table.update()

    def onListItemDoubleClicked(self, sender, sel, ptr):
        # 获取当前选中的 item
        selected_item = self.List_3.getCurrentItem()
        if selected_item:
            # 获取 item 的文本内容
            item_text = self.List_3.getItemText(selected_item)

            # 将 item 的文本内容写入 AFXTextField
            self.form.keyword65Kw.setValue(str(item_text))

            # 如果需要，可以在消息区域显示选中的内容
            mw = getAFXApp().getAFXMainWindow()
            mw.writeToMessageArea("Selected item: " + str(item_text))


    def getTable(self):
        return self.table

    def getComboBox14(self):
        return self.ComboBox_14

    def getList3(self):
        return self.List_3
###########################################################################
# Class definition
###########################################################################

class STPM_test1033DBFileHandler(FXObject):

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def __init__(self, form, keyword, patterns='*'):
        
        self.form = form
        self.patterns = patterns
        self.patternTgt = AFXIntTarget(0)
        exec('self.fileNameKw = form.%sKw' % keyword)
        self.readOnlyKw = AFXBoolKeyword(None, 'readOnly', AFXBoolKeyword.TRUE_FALSE)
        FXObject.__init__(self)
        FXMAPFUNC(self, SEL_COMMAND, AFXMode.ID_ACTIVATE, STPM_test1033DBFileHandler.activate)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def activate(self, sender, sel, ptr):

       fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
           self.fileNameKw, self.readOnlyKw,
           AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
       fileDb.setReadOnlyPatterns('*.odb')
       fileDb.create()
       fileDb.showModal()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # def outputFilePath(self, sender, sel, ptr):
    #     selectedFilePath = self.fileNameKw.getValue()
    #     mw = getAFXApp().getAFXMainWindow()
    #     mw.writeToMessageArea('Selected file path:'+ str(selectedFilePath))
    #     with open(selectedFilePath, 'r') as f:
    #         self.db.materials_data
    
    

       
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
        # 设置默认路径
        if not self.JSONNameKw.getValue():
            default_path = r"D:\psh"        
            self.JSONNameKw.setValue(default_path)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.JSONNameKw ,self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
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
            self.db.materials_data =  json.load(f, object_pairs_hook=OrderedDict)        
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
        # 设置默认路径
        if not self.fileNameKw.getValue():
            default_path = r"D:\桌面"
            self.fileNameKw.setValue(default_path)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.fileNameKw, self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
        fileDb.setReadOnlyPatterns('*.odb')
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
            table.setLeadingRowLabels('para\tvalue\ttype\tpart\tfeature')

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
        # 设置默认路径
        if not self.InputDataName.getValue():
            default_path = r"D:\桌面"
            self.InputDataName.setValue(default_path)
        fileDb = AFXFileSelectorDialog(getAFXApp().getAFXMainWindow(), 'Select a File',
                                       self.InputDataName, self.readOnlyKw,
                                       AFXSELECTFILE_ANY, self.patterns, self.patternTgt)
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
        try:
            # 读取 Excel 文件
            workbook = xlrd.open_workbook(selectedFilePath)
            # 获取所有 Sheet 名称
            sheet_names = workbook.sheet_names()

            # 清空 List_3 的内容
            if list:
                list.clearItems()

            for sheet_name in sheet_names:
                list.appendItem(text=str(sheet_name))
            list.appendItem(text='HOLDING')

        except Exception as e:
            mw.writeToMessageArea("Error reading Excel file: " + str(e))