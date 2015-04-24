#!/usr/bin/env python

import wx
import os

"""This is were we set up a box where you enter the
   q-file and grid name  
"""

class Convq(wx.Panel):

    def __init__(self, parent, ID, label, label2, initval):
        wx.Panel.__init__(self, parent, ID)
        
        self.value = initval
        self.value2 = 1
        box = wx.StaticBox(self, -1, label)
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        
        self.manual_text = wx.TextCtrl(self, -1, 
            size=(80,-1),
            value=initval,
            style=wx.TE_PROCESS_ENTER)

        self.manual_text2 = wx.TextCtrl(self, -1, 
            size=(50,-1),
            value=str(1),
            style=wx.TE_PROCESS_ENTER)

                            
        self.Bind(wx.EVT_TEXT_ENTER, self.on_text_enter, self.manual_text)
        self.Bind(wx.EVT_TEXT_ENTER, self.on_text_enter, self.manual_text2)
        self.s_label = wx.StaticText(self, -1 , label=label2) 
        self.browse = wx.Button( self, -1, label="Browse")  
        
        manual_box = wx.BoxSizer(wx.HORIZONTAL)
        manual_box.Add(self.manual_text, flag=wx.ALIGN_CENTER_VERTICAL)
        manual_box.Add(self.browse, flag=wx.ALIGN_CENTER_VERTICAL)
        manual_box.Add(self.s_label, flag=wx.ALIGN_CENTER_VERTICAL)
        manual_box.Add(self.manual_text2, flag=wx.ALIGN_CENTER_VERTICAL)
        
 #       self.browse = wx.Button( self, wx.ID_ANY, "Browse", 
 #                        wx.DefaultPosition, wx.DefaultSize, -1 )
        self.Bind( wx.EVT_BUTTON, self.sel_file, self.browse )
        
        sizer.Add(manual_box, 0, wx.ALL, 10)
        
        self.SetSizer(sizer)
        sizer.Fit(self)
   
    def on_text_enter(self, event):
        self.value = self.manual_text.GetValue()
        self.value2 = self.manual_text2.GetValue()
    def is_auto(self):
        return self.radio_auto.GetValue()
        
    def manual_value(self):
        return self.value

    def second_value(self):
        return self.manual_text2.GetValue()

    def sel_file(self,event):
	   wildcard = "All files (*.*)|*.*"
	   dialog = wx.FileDialog(None, "Choose a file", os.getcwd(), "", wildcard, wx.OPEN)	
	   if dialog.ShowModal() == wx.ID_OK:
            self.manual_text.SetValue(dialog.GetPath())
    """ --- ----------------------------------------------------------------------- """
    """ This is the box that will allow us to change the J, K, L indexes to plot 
    """

class IndexBox(wx.Panel):

    def __init__(self, parent, ID, label, initval1, initval2):
        wx.Panel.__init__(self, parent, ID)
        self.value1 = initval1
        self.value2 = initval2
        
        box = wx.StaticBox(self, -1, label)
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        
        self.max_label = wx.StaticText(self, -1 , label= " Max: ")  
        self.min_label = wx.StaticText(self, -1 , label= "Min: ") 

        self.manual_text1 = wx.SpinCtrl(self, -1, 
            size=(65,-1),
            value=str(initval1),
            style=wx.TE_PROCESS_ENTER,
            name = "Min",
            max=9999,min=1)
        self.manual_text2 = wx.SpinCtrl(self, -1, 
            size=(65,-1),
            value=str(initval2),
            style=wx.TE_PROCESS_ENTER,
            name = 'Max',
            max=9999)           
  

        self.Bind(wx.EVT_TEXT_ENTER, self.on_text_enter1, self.manual_text1)
        self.Bind(wx.EVT_TEXT_ENTER, self.on_text_enter2, self.manual_text2)
        
        manual_box = wx.BoxSizer(wx.HORIZONTAL)
        manual_box.Add(self.min_label, flag=wx.ALIGN_CENTER_VERTICAL)
        manual_box.Add(self.manual_text1, flag=wx.ALIGN_CENTER_VERTICAL)
        manual_box.Add(self.max_label, flag=wx.ALIGN_CENTER_VERTICAL)
        manual_box.Add(self.manual_text2, flag=wx.ALIGN_CENTER_VERTICAL)      
        sizer.Add(manual_box, 0, wx.ALL, 10)
        self.SetSizer(sizer)
        sizer.Fit(self)


    def on_text_enter1(self, event):
        self.value1 = self.manual_text1.GetValue()

    def on_text_enter2(self, event):
        self.value2 = self.manual_text2.GetValue()
    
    def is_auto(self):
        return self.radio_auto.GetValue()
        
    def manual_value1(self):
        return self.value1

    def manual_value2(self):
        return self.value2
  
    def blank_out(self, active):  
        accept = True
        connum = self.manual_text1.GetValue()
        if (active == 1):
            #self.manual_text2.SetValue(connum)
            accept = False       

        self.manual_text2.Enable(accept)
    
       
 
    """ --------------------------------------------------------------------------"""
    """ Here is where we choose what plane to loop through """

class WhichPlane(wx.Panel):
    

    def __init__(self, parent, ID, label, initval1, initval2):
        wx.Panel.__init__(self, parent, ID)
        
        self.value1 = initval1
        self.value2 = initval2
        
        box = wx.StaticBox(self, -1, label)
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
             
        self.j_sel = wx.RadioButton(self, -1, 
            label="Const-J ", style=wx.RB_GROUP)
        self.k_sel = wx.RadioButton(self, -1, 
            label="Const-K ")
        self.l_sel = wx.RadioButton(self, -1, 
            label="Const-L ")

#        self.j_sel.Bind(wx.EVT_RADIOBUTTON, self.SetVal)
#        self.k_sel.Bind(wx.EVT_RADIOBUTTON, self.SetVal)
#        self.l_sel.Bind(wx.EVT_RADIOBUTTON, self.SetVal)

        manual_box = wx.BoxSizer(wx.VERTICAL)
        manual_box.Add(self.j_sel, flag=wx.ALIGN_CENTER_VERTICAL)
        manual_box.AddSpacer(53)
        manual_box.Add(self.k_sel, flag=wx.ALIGN_CENTER_VERTICAL)
        manual_box.AddSpacer(53)
        manual_box.Add(self.l_sel, flag=wx.ALIGN_CENTER_VERTICAL)
     
        sizer.Add(manual_box, 0, wx.ALL, 10)
        self.SetSizer(sizer)
        sizer.Fit(self)

#    def SetVal(self, e):
#        hmmm = 1 


    """ -------------------------------------------------------------------------- """
    """ -------------------------------------------------------------------------- """
    """ Create a drop down menu to select the q variable you want to plot """
class QvarBox(wx.Panel):

    def __init__(self, parent, ID, label1, initval):
        wx.Panel.__init__(self, parent, ID)
       
        qvars = [ 'Density' , 'Streamwise (u) Velocity' , 'Spanwise (v) Velocity' , 'Normal (w) Velocity',
                  'k (SST) ' , 'w (SST) ' , 'Intermitency (SST_LM)' , 'RE_thetat (SST_LM)',
                  'A_r (SST_LM_RA)', 'Pressure', 'Velocity Magnitude', 'Velocity Vectors' ]
        self.qvarnum = [ '1' , '2', '3' , '4', '7', '8' ,'9', '10','11', '15', '16', '17' ] 
        self.cb = wx.ComboBox(self, choices=qvars , style=wx.CB_READONLY) 

        self.num = int(self.qvarnum[1])  
        self.cb.Bind(wx.EVT_COMBOBOX, self.OnSelect)

    def OnSelect(self, e):
        i = e.GetString()
        self.num = int(self.qvarnum[self.cb.GetCurrentSelection()])         
    
    def GetValue(self):
        return self.cb.GetValue()

    def SetString(self,string):
        self.cb.SetStringSelection(string)
    
    """ --------------------------------------------------------------------------"""
    """ -------------------------------------------------------------------------- """
    """ Create a drop down menu to select the resolution of the contour plot """
class ResBox(wx.Panel):

    def __init__(self, parent, ID, label1, initval):
        wx.Panel.__init__(self, parent, ID)
       
        res = [ 'Very Low', 'Low', 'Medium', 'High', 'Very High' ]
        self.resnum = [ '30', '60' , '100' , '140' , '250' ] 
        self.cb = wx.ComboBox(self, choices=res, style=wx.CB_READONLY) 

        self.num = int(self.resnum[1])  
        self.cb.Bind(wx.EVT_COMBOBOX, self.OnSelect)

    def OnSelect(self, e):
        i = e.GetString()
        self.num = int(self.resnum[self.cb.GetCurrentSelection()]) 

    def GetValue(self):
        return self.cb.GetValue()

    def SetString(self,string):
        self.cb.SetStringSelection(string)        
        


class GetBoundaryLayer(wx.Panel):
    def __init__(self, parent, ID, label1, initval):
        wx.Panel.__init__(self, parent, ID)
            


class BoundControlBox(wx.Panel):
    """ A static box with a couple of radio buttons and a text
        box. Allows to switch between an automatic mode and a 
        manual mode with an associated value.
    """
    def __init__(self, parent, ID, label, initval):
        wx.Panel.__init__(self, parent, ID)
        
        self.value = initval
        
        box = wx.StaticBox(self, -1, label)
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        
        self.radio_auto = wx.RadioButton(self, -1, 
            label="Auto", style=wx.RB_GROUP)
        self.radio_manual = wx.RadioButton(self, -1,
            label="Manual")
        self.manual_text = wx.TextCtrl(self, -1, 
            size=(35,-1),
            value=str(initval),
            style=wx.TE_PROCESS_ENTER)
        
        self.Bind(wx.EVT_UPDATE_UI, self.on_update_manual_text, self.manual_text)
        self.Bind(wx.EVT_TEXT_ENTER, self.on_text_enter, self.manual_text)
        
        manual_box = wx.BoxSizer(wx.HORIZONTAL)
        manual_box.Add(self.radio_manual, flag=wx.ALIGN_CENTER_VERTICAL)
        manual_box.Add(self.manual_text, flag=wx.ALIGN_CENTER_VERTICAL)
        
        sizer.Add(self.radio_auto, 0, wx.ALL, 10)
        sizer.Add(manual_box, 0, wx.ALL, 10)
        
        self.SetSizer(sizer)
        sizer.Fit(self)
    
    def on_update_manual_text(self, event):
        self.manual_text.Enable(self.radio_manual.GetValue())
 
    def on_text_enter(self, event):
        self.value = self.manual_text.GetValue()
    
    def is_auto(self):
        return self.radio_auto.GetValue()
        
    def manual_value(self):
        return self.value




