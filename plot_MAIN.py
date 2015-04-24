#!/usr/bin/env python
from gui_classes import *
from p3dmod import *
#from plot_funcs import *
import os
import wx
import subprocess
from subprocess import Popen, PIPE

# The recommended way to use wx with mpl is with the WXAgg
# backend.
#
import matplotlib
matplotlib.use('WXAgg')
from matplotlib.figure import Figure
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigCanvas
import numpy as np
import pylab
from pylab import arange, meshgrid, contourf, quiver, plot, colorbar


class GraphFrame(wx.Frame):
    """ The main frame of the application """

    title = 'Contour Plot Generator'

    def __init__(self):
        wx.Frame.__init__(self, None, -1, self.title)

        self.update       = False
        self.bldraw       = False
        self.blcall       = False
        self.multgrd      = False
        self.redrawgrid   = True 
        self.newgrid      = True
        self.newq         = True
        self.changebounds = False
        self.line = ""
        self.cb  = ""
        self.cp  = 1
        self.cpn = 1
        self.create_menu()
        self.create_status_bar()
        self.create_main_panel()
        
        self.redraw_timer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER, self.on_redraw_timer, self.redraw_timer)
        self.redraw_timer.Start(100)

    def create_menu(self):
        self.menubar = wx.MenuBar()

        menu_file = wx.Menu()
        m_expt = menu_file.Append(-1, "&Save plot\tCtrl-S", "Save plot to file")
        self.Bind(wx.EVT_MENU, self.on_save_plot, m_expt)
        menu_file.AppendSeparator()
        m_exit = menu_file.Append(-1, "E&xit\tCtrl-X", "Exit")
        self.Bind(wx.EVT_MENU, self.on_exit, m_exit)

        self.menubar.Append(menu_file, "&File")
        self.SetMenuBar(self.menubar)

    def create_main_panel(self):
        self.panel = wx.Panel(self)
        self.init_contour()
        self.boundary_layer()
        self.canvas2           = FigCanvas(self.panel, -1, self.fig1)
        self.canvas3           = FigCanvas(self.panel, -1, self.fig2)
        self.xmin_control      = BoundControlBox(self.panel, -1, "X min", 0)
        self.xmax_control      = BoundControlBox(self.panel, -1, "X max", 1)
        self.ymin_control      = BoundControlBox(self.panel, -1, "Y min", 0)
        self.ymax_control      = BoundControlBox(self.panel, -1, "Y max", 1)
        self.qfile_control     = Convq(self.panel, -1, "Enter Q file", " Grid #: ", "q.save")
        self.grid_file_control = Convq(self.panel, -1, "Enter Grid file", " Chord: ", "grid.in")
        self.j_index_control   = IndexBox(self.panel, -1, "Enter J Indices", 1, -1)
        self.k_index_control   = IndexBox(self.panel, -1, "Enter K Indices", 1, -1)
        self.l_index_control   = IndexBox(self.panel, -1, "Enter L Indices", 1, -1)
        self.qcont_label       = wx.StaticText(self.panel, -1 , label="Select Q variable")
        self.selectq_control   = QvarBox(self.panel, -1, -1, 1)
        self.res_label         = wx.StaticText(self.panel, -1 , label="Select Contour Resolution")
        self.res_control       = ResBox(self.panel, -1, "Select Contour Resolution", 1)
        self.ref_mach          = wx.StaticText(self.panel, -1 , label= "Mach:                   ")
        self.times             = wx.StaticText(self.panel, -1 , label= "x")
        self.Alpha             = wx.StaticText(self.panel, -1 , label= "Angle of Attach:           ")
        self.Rey               = wx.StaticText(self.panel, -1 , label= "Reynolds #:                       ")
        self.Njind             = wx.StaticText(self.panel, -1 , label= "J-Dim:      ")
        self.Nkind             = wx.StaticText(self.panel, -1 , label= "K-Dim:      ")
        self.Nlind             = wx.StaticText(self.panel, -1 , label= "L-Dim:      ")

        self.qfile_control.manual_text2.Enable(0)

        self.j_index_control.Bind(wx.EVT_SPINCTRL, self.get_c_plane,
                                  self.j_index_control.manual_text1)
        self.k_index_control.Bind(wx.EVT_SPINCTRL, self.get_c_plane,
                                  self.k_index_control.manual_text1)
        self.l_index_control.Bind(wx.EVT_SPINCTRL, self.get_c_plane,
                                  self.l_index_control.manual_text1)


        self.update_button = wx.Button(self.panel, -1, "Update")
        self.Bind(wx.EVT_BUTTON, self.on_update_button, self.update_button)
 #       self.Bind(wx.EVT_UPDATE_UI, self.on_update_update_button, self.update_button)

        self.bl_button   = wx.Button(self.panel, -1, "Draw BL Profile")
        self.Bind(wx.EVT_BUTTON, self.on_bl_button, self.bl_button)
        self.stream_ind  = wx.StaticText(self.panel, -1 , label= "Streamwise Index")
        self.norm_ind    = wx.StaticText(self.panel, -1 , label= "Max K Index")
        self.x_over_c    = wx.StaticText(self.panel, -1 , label= "x/c")
        self.mom_label   = wx.StaticText(self.panel, -1 , label= " ")
        self.shape_label = wx.StaticText(self.panel, -1 , label= " ")
        self.Bl_jplane   = wx.SpinCtrl(self.panel, -1, size=(65,-1), value=str(1),
                                       style=wx.TE_PROCESS_ENTER,
                                       name = "Min", max=9999, min=1)
        self.Bind(wx.EVT_SPINCTRL, self.on_bl_button, self.Bl_jplane)
        self.Bl_kplane   = wx.SpinCtrl(self.panel, -1, size=(65,-1), value=str(80),
                                       style=wx.TE_PROCESS_ENTER,
                                       name = "Min", max=9999, min=1)
        self.Bind(wx.EVT_SPINCTRL, self.on_bl_button, self.Bl_kplane)

        self.cb_stretch = wx.CheckBox(self.panel, -1, "Stretch K",
                                   style=wx.ALIGN_RIGHT)
        self.Bind(wx.EVT_CHECKBOX, self.on_cb_stretch, self.cb_stretch)
        self.cb_stretch.SetValue(False)
        self.mom_norm = wx.CheckBox(self.panel, -1,
                                    "Normalize by \nMomentum Thickness",
                                    style=wx.ALIGN_RIGHT)
        self.Bind(wx.EVT_CHECKBOX, self.on_mom_norm, self.mom_norm)
        self.mom_norm.SetValue(False)
        self.k_str = wx.SpinCtrl(self.panel, -1, size=(65,-1), value=str(1),
                                     style=wx.TE_PROCESS_ENTER,
                                     name = "Min", max=9999, min=1)
        
        self.cb_grid = wx.CheckBox(self.panel, -1,
                                   "Show Grid",
                                   style=wx.ALIGN_RIGHT)
        self.Bind(wx.EVT_CHECKBOX, self.on_cb_grid, self.cb_grid)
        self.cb_grid.SetValue(False)


        self.flatten = wx.CheckBox(self.panel, -1,
                                   "Flatten Geometry",
                                    style=wx.ALIGN_RIGHT)
        self.Bind(wx.EVT_CHECKBOX, self.on_flatten, self.flatten)



        self.l_sel = wx.RadioButton(self.panel, -1,
            label="Const-L ", style=wx.RB_GROUP)
        self.j_sel = wx.RadioButton(self.panel, -1,
            label="Const-J ")
        self.k_sel = wx.RadioButton(self.panel, -1,
            label="Const-K ")
        self.j_sel.Bind(wx.EVT_RADIOBUTTON, self.get_c_plane)
        self.k_sel.Bind(wx.EVT_RADIOBUTTON, self.get_c_plane)
        self.l_sel.Bind(wx.EVT_RADIOBUTTON, self.get_c_plane)
        if not self.update:
            self.l_index_control.blank_out(1)

#        self.Bind(wx.EVT_CHAR_HOOK,self.onKeyPress)   
#        self.xmin_control.radio_auto.Bind(wx.EVT_RADIOBUTTON,self.on_update_button(self))



        self.selbox = wx.BoxSizer(wx.VERTICAL)
        self.selbox.AddSpacer(20)
        self.selbox.Add(self.j_sel, border=5, flag=wx.ALL )
        self.selbox.AddSpacer(45)
        self.selbox.Add(self.k_sel, border=5, flag=wx.ALL )
        self.selbox.AddSpacer(45)
        self.selbox.Add(self.l_sel, border=5, flag=wx.ALL )

        self.hbox1 = wx.BoxSizer(wx.HORIZONTAL)
        self.hbox1.AddSpacer(35)
        self.hbox1.Add(self.ref_mach, border=5, flag=wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        self.hbox1.Add(self.Alpha, border=5, flag=wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        self.hbox1.Add(self.Rey, border=5, flag=wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        self.hbox1.Add(self.Njind, border=5, flag=wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        self.hbox1.Add(self.Nkind, border=5, flag=wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        self.hbox1.Add(self.Nlind, border=5, flag=wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        self.hbox1.AddSpacer(20)
        self.hbox1.Add(self.cb_stretch, border=5, flag=wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        self.hbox1.Add(self.times, border=5, flag=wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        self.hbox1.Add(self.k_str, border=5, flag=wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        self.hbox1.AddSpacer(10)
        self.hbox1.Add(self.flatten, border=5, flag=wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        self.hbox1.AddSpacer(10)
        self.hbox1.Add(self.cb_grid, border=5, flag=wx.ALL | wx.ALIGN_CENTER_VERTICAL)


        self.hbox5 = wx.BoxSizer(wx.HORIZONTAL)
        self.hbox5.Add(self.xmin_control, border=5, flag=wx.ALL)
        self.hbox5.Add(self.xmax_control, border=5, flag=wx.ALL)

        self.hbox6 = wx.BoxSizer(wx.HORIZONTAL)
        self.hbox6.Add(self.ymin_control, border=5, flag=wx.ALL)
        self.hbox6.Add(self.ymax_control, border=5, flag=wx.ALL)

        self.hbox7 = wx.BoxSizer(wx.VERTICAL)
        self.hbox7.Add(self.hbox5, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.hbox7.Add(self.hbox6, 0, flag=wx.ALIGN_LEFT | wx.TOP)

        self.hbox2 = wx.BoxSizer(wx.VERTICAL)
        self.hbox2.Add(self.grid_file_control, border=5, flag=wx.ALL)
        self.hbox2.AddSpacer(5)
        self.hbox2.Add(self.qfile_control, border=5, flag=wx.ALL)
        self.hbox2.AddSpacer(5)
        self.hbox2.Add(self.qcont_label, border=5, flag=wx.ALL)
        self.hbox2.Add(self.selectq_control, border=5, flag=wx.ALL)
        self.hbox2.Add(self.res_label, border=5, flag=wx.ALL)
        self.hbox2.Add(self.res_control, border=5, flag=wx.ALL)

        self.hbox3 = wx.BoxSizer(wx.VERTICAL)
        self.hbox3.Add(self.j_index_control, border=5, flag=wx.ALL )
        self.hbox3.Add(self.k_index_control, border=5, flag=wx.ALL )
        self.hbox3.Add(self.l_index_control, border=5, flag=wx.ALL )
        self.hbox3.Add(self.update_button, border=5, flag=wx.ALL  | wx.ALIGN_CENTER_HORIZONTAL)

        self.blsel = wx.BoxSizer(wx.VERTICAL)
        self.blsel.Add(self.bl_button, 0, flag=wx.ALIGN_CENTER | wx.TOP)
        self.blsel.AddSpacer(15)
        self.blsel.Add(self.x_over_c, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.blsel.AddSpacer(15)
        self.blsel.Add(self.stream_ind, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.blsel.Add(self.Bl_jplane, 0, flag=wx.ALIGN_CENTER | wx.TOP)
        self.blsel.AddSpacer(10)
        self.blsel.Add(self.norm_ind, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.blsel.Add(self.Bl_kplane, 0, flag=wx.ALIGN_CENTER | wx.TOP)
        self.blsel.AddSpacer(10)
        self.blsel.Add(self.mom_norm, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.blsel.AddSpacer(20)
        self.blsel.Add(self.mom_label, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.blsel.AddSpacer(25)
        self.blsel.Add(self.shape_label, 0, flag=wx.ALIGN_LEFT | wx.TOP)


        self.hbox4 = wx.BoxSizer(wx.HORIZONTAL)
        self.hbox4.AddSpacer(10)
        self.hbox4.Add(self.hbox2, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.hbox4.AddSpacer(1)
        self.hbox4.Add(self.hbox3, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.hbox4.Add(self.selbox, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.hbox4.AddSpacer(5)
        self.hbox4.Add(self.hbox7, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.hbox4.AddSpacer(5)
        self.hbox4.Add(self.blsel, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.hbox4.AddSpacer(25)
        self.hbox4.Add(self.canvas3, 1, flag=wx.ALIGN_LEFT | wx.TOP | wx.GROW)


        self.vbox = wx.BoxSizer(wx.VERTICAL)
        self.vbox.Add(self.canvas2, 1, flag=wx.LEFT | wx.TOP | wx.GROW)
        self.vbox.Add(self.hbox1, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        self.vbox.Add(self.hbox4, 0, flag=wx.ALIGN_LEFT | wx.TOP)

        self.panel.SetSizer(self.vbox)
        self.vbox.Fit(self)

        # Default to streamwise velocity and low resolution

        if self.selectq_control.GetValue() == '':
            self.selectq_control.SetString(string='Streamwise (u) Velocity')

        if self.res_control.GetValue() == '':
            self.res_control.SetString(string='Low')


    def create_status_bar(self):
        self.statusbar = self.CreateStatusBar()

    def init_plot(self):
        self.dpi = 100
        self.fig = Figure((3.0, 3.0), dpi=self.dpi)
        self.axes = self.fig.add_subplot(111)
        self.axes.set_axis_bgcolor('white')


#   Determine which plane to hold constant
    def get_c_plane(self,event):
        if (self.j_sel.GetValue()):
             self.cp  = 3
             self.cpn = self.j_index_control.manual_text1.GetValue()
             self.nx  = self.nkind
             self.ny  = self.nlind
        if (self.k_sel.GetValue()):
             self.cp  = 2
             self.cpn = self.k_index_control.manual_text1.GetValue()
             self.nx  = self.njind
             self.ny  = self.nlind
        if (self.l_sel.GetValue()):
             self.cp  = 1
             self.cpn = self.l_index_control.manual_text1.GetValue()
             self.nx  = self.njind
             self.ny  = self.nkind 
        self.j_index_control.blank_out(self.j_sel.GetValue())
        self.k_index_control.blank_out(self.k_sel.GetValue())
        self.l_index_control.blank_out(self.l_sel.GetValue())
#        if (self.cpt != self.cp):
#             if self.cpt == 1:
#                 self.l_index_control.manual_text2.SetValue(self.nlind)
#                 self.l_index_control.manual_text1.SetValue(1)
#             elif self.cpt == 2:
#                 self.k_index_control.manual_text2.SetValue(self.nkind)
#                 self.k_index_control.manual_text1.SetValue(1)
#             elif self.cpt == 3:
#                 self.j_index_control.manual_text2.SetValue(self.njind)
#                 self.j_index_control.manual_text1.SetValue(1)
#        self.cpt = self.cp    

    # Initialize the main figure for the contour plot
    def init_contour(self):
        self.data2 = []
        self.x          = arange(0,10,.5)
        self.y          = arange(0,10,.5)
        self.X, self.Y  = pylab.meshgrid(self.x,self.y)
        self.Z, self.Zt = pylab.meshgrid(self.x,self.y)
        self.dpi2       = 101
        self.fig1       = Figure((3.0, 3.0), dpi=self.dpi2)
        self.axes2      = self.fig1.add_subplot(111)
        self.axes2.set_axis_bgcolor('white')
        if not self.update:
            initplot = self.cplot = self.axes2.contourf(self.X,self.Y,self.Z)
            self.cb  = self.fig1.colorbar(initplot)



    def on_update_button(self, event):
        self.j_index_control.value1 = self.j_index_control.manual_text1.GetValue()
        self.j_index_control.value2 = self.j_index_control.manual_text2.GetValue()
        self.k_index_control.value1 = self.k_index_control.manual_text1.GetValue()
        self.k_index_control.value2 = self.k_index_control.manual_text2.GetValue()
        self.l_index_control.value1 = self.l_index_control.manual_text1.GetValue()
        self.l_index_control.value2 = self.l_index_control.manual_text2.GetValue()
        self.qfile_control.value    = self.qfile_control.manual_text.GetValue()
        self.grid_file_control.value  = self.grid_file_control.manual_text.GetValue()
        self.grid_file_control.value2 = self.grid_file_control.manual_text2.GetValue()
        self.qfile_control.value2   = self.grid_file_control.manual_text2.GetValue()
        self.change = True
        

        # See if anything has changed since last time update button was pushed
        if (self.update):
            self.get_c_plane(self)
            # Determine if the bounds of the axis have changed 
            if (self.axmin  != self.xmin_control.is_auto() or
                self.axmax  != self.xmax_control.is_auto() or 
                self.aymin  != self.ymin_control.is_auto() or 
                self.aymax  != self.ymax_control.is_auto() or 
                self.xmin   != self.xmin_control.manual_value() or
                self.xmax   != self.xmax_control.manual_value() or
                self.ymin   != self.ymin_control.manual_value() or
                self.ymax   != self.ymax_control.manual_value()):
                    self.changebounds = True
                    
            if (self.jmin == self.j_index_control.manual_value1() and
                self.jmax == self.j_index_control.manual_value2() and
                self.kmin == self.k_index_control.manual_value1() and
                self.kmax == self.k_index_control.manual_value2() and
                self.lmin == self.l_index_control.manual_value1() and
                self.lmax == self.l_index_control.manual_value2() and
                self.selvar == self.selectq_control.num           and
                self.qfile  == self.qfile_control.manual_value()  and
                self.strk == self.cb_stretch.IsChecked()             and
                self.flat == self.flatten.IsChecked()             and
                self.kstretch_fac == self.k_str.GetValue()        and
                self.res  ==self.res_control.GetValue()           and
                self.gnum == self.qfile_control.manual_text2.GetValue() and
                self.grid == self.grid_file_control.manual_value()):
                    self.change = False
            # See if the plane we are looking at has changed
            if (self.cpt != self.cp or self.cpnt != self.cpn):
                self.newgrid = True
                self.newq    = True 
                self.change  = True 
            # Now see if we specifically need to reload grid cords or q-var
            if (self.gnum != self.qfile_control.manual_text2.GetValue() or
                self.grid != self.grid_file_control.manual_value() ):
                    self.newgrid = True
            if (self.gnum   != self.qfile_control.manual_text2.GetValue() or
                self.qfile  != self.qfile_control.manual_value()          or
                self.selvar != self.selectq_control.num):
                    self.newq = True


        
        self.grid   = self.grid_file_control.manual_value()
        self.qfile  = self.qfile_control.manual_value()
        self.jmin   = self.j_index_control.manual_value1()
        self.jmax   = self.j_index_control.manual_value2()
        self.kmin   = self.k_index_control.manual_value1()
        self.kmax   = self.k_index_control.manual_value2()
        self.lmin   = self.l_index_control.manual_value1()
        self.lmax   = self.l_index_control.manual_value2()
        self.axmin  = self.xmin_control.is_auto()
        self.axmax  = self.xmax_control.is_auto()
        self.aymin  = self.ymin_control.is_auto()
        self.aymax  = self.ymax_control.is_auto()
        self.xmin   = self.xmin_control.manual_value() 
        self.xmax   = self.xmax_control.manual_value() 
        self.ymin   = self.ymin_control.manual_value() 
        self.ymax   = self.ymax_control.manual_value()
        self.strk   = self.cb_stretch.IsChecked()
        self.flat   = self.flatten.IsChecked()
        self.selvar = self.selectq_control.num
        self.kstretch_fac = self.k_str.GetValue()
        self.res    = self.res_control.GetValue()
        self.gnum   = self.qfile_control.manual_text2.GetValue()
        self.cpt    = self.cp 
        self.cpnt   = self.cpn 
        self.cord   = self.grid_file_control.second_value() 
        if (self.bldraw == False and self.blcall == True):
            self.update = False
        # If anything has changed re-read the header file  
        if (self.update == False or self.change == True):
           self.ngrid,self.njind,self.nkind,self.nlind,self.nq,self.nqc,\
           self.refmach,self.alpha,self.rey,self.time,self.gaminf,self.tinf, \
           self.beta,self.fsmach = readheader(self.qfile,self.gnum)  
           self.get_c_plane(self)
                   
        # If we need to reload the grid coordinates
        if (self.newgrid):
            self.X, self.Y = getgridcords(self.grid,self.gnum,self.njind,
                                           self.nkind,self.nlind,self.cp,
                                           self.cpn,self.nx,self.ny)
            self.gminx = np.amin(self.X)
            self.gmaxx = np.amax(self.X)
            self.gminy = np.amin(self.Y)
            self.gmaxy = np.amax(self.Y)                                      
            self.newgrid = False 
        # If we need to reload a q-var 
        # Special case of velocity vectors 
        if (self.newq):
            if self.selvar == 17:
                qnum = 2
                self.u,self.v =readqvel(self.qfile,self.ngrid,self.gnum,
                               self.njind,self.nkind,self.nlind,self.nq,
                               self.nx,self.ny,self.cp,self.cpn)
            else:
                qnum = self.selvar
        
            self.Q = readq(self.qfile,self.ngrid,self.gnum,self.njind,
                           self.nkind,self.nlind,self.nq,qnum, 
                           self.nx,self.ny,self.cp,self.cpn)
            self.minz = np.amin(self.Q)
            self.maxz = np.amax(self.Q)
            self.newq = False 
        if (self.update == False or self.change == True):
            self.draw_update()
        # If we are only changing the window             
        if (self.changebounds and not self.change):
            self.check_auto_axis(-0.25,1.25,-0.25,0.25)
            self.canvas2.draw()
        # If this is the first time through load the max indicies
        if (self.update == False):
            self.j_index_control.manual_text2.SetValue(self.njind)
            self.k_index_control.manual_text2.SetValue(self.nkind)
            self.l_index_control.manual_text2.SetValue(self.nlind)            
        self.update = True 


    def draw_update(self):
        # Enable/Disable multi-grid selection 
        if (self.ngrid > 1):
            self.multgrd = True
            self.qfile_control.manual_text2.Enable(1)
        else:
            self.multgrd = False
            self.qfile_control.manual_text2.Enable(0)

        # Write information about the Mach #, AoA, Rey #, etc
        self.ref_mach.SetLabel( ''.join([' Mach: ', str(self.refmach)]) )
        self.Alpha.SetLabel( ''.join(['Angle of Attach: ', str(self.alpha)]) )
        self.Rey.SetLabel( ''.join(['  Reynolds #: ', str(self.rey)]) )
        self.Njind.SetLabel( ''.join(['  J-dim:  ', str(self.njind)]) )
        self.Nkind.SetLabel( ''.join(['    K-dim:  ', str(self.nkind)]) )
        self.Nlind.SetLabel( ''.join(['     L-dim:  ', str(self.nlind)]) )

        # Get the range of the q-var for the contour plot
        res = self.res_control.num 
        sp = (self.maxz - self.minz)/res
        if (self.minz >= 0):
            levels = arange((self.minz-self.minz*.1),self.maxz*1.05, sp )
        else:
            levels = arange((self.minz+self.minz*.1),self.maxz*1.05, sp ) 


        self.Xplt = self.X
        self.Yplt = self.Y 
        self.axes2.cla()  
        if self.cb_stretch.IsChecked():
            stretch = int(self.k_str.GetValue())
            lj = len(self.X[:,0])
            lk = len(self.X[0,:])
            self.Xplt,self.Yplt = stretch_norm(self.X,self.Y,stretch,lj,lk)
            self.Xplt = np.array(self.Xplt)
            self.Yplt = np.array(self.Yplt)
        self.cplot = self.axes2.contourf(self.Xplt,self.Yplt,self.Q,levels)
        
        if (self.selvar == 17):
            self.axes2.quiver(self.Xplt[::6,::3],self.Yplt[::6,::3],         
                             self.u[::6,::3],self.v[::6,::3],scale=5.0,
                             width=.001)
        # This is a pretty good default window size
        self.check_auto_axis(-0.25,1.25,-0.25,0.25)
        self.canvas2.draw()
        
        if (self.cb_grid.IsChecked() and self.redrawgrid):
            self.show_grid()
        
    def show_grid(self):
        if( self.cb_grid.IsChecked()):
            for j in np.arange(0,int(self.njind),2):
                self.axes2.plot(self.Xplt[j,:],self.Yplt[j,:],color='k',linewidth=0.25)
            for k in np.arange(0,int(self.nkind),2):
                self.axes2.plot(self.Xplt[:,k],self.Yplt[:,k],color='k',linewidth=0.25)
            self.check_auto_axis(-0.25,1.25,-0.25,0.25)
            self.canvas2.draw()
        else:
             self.draw_update()

    def check_auto_axis(self,minx,maxx,miny,maxy):
        self.xmax_control.value = self.xmax_control.manual_text.GetValue()
        self.xmin_control.value = self.xmin_control.manual_text.GetValue()
        self.ymax_control.value = self.ymax_control.manual_text.GetValue()
        self.ymin_control.value = self.ymin_control.manual_text.GetValue()

        # Since we are calling (-0.25,1.25,-0.25,0.25) as the default size
        # make sure there will not be a bunch of blank space          
        if (self.gminx > minx): minx = self.gminx 
        if (self.gmaxx < maxx): maxx = self.gmaxx 
        if (self.gminy > miny): miny = self.gminy 
        if (self.gmaxy < maxy): maxy = self.gmaxy      
        
        if self.xmax_control.is_auto():
            xmax = maxx
        else:
            xmax = float(self.xmax_control.manual_value())
        if self.xmin_control.is_auto():
            xmin = minx
        else:
            xmin = float(self.xmin_control.manual_value())
        if self.ymin_control.is_auto():
            ymin = miny
        else:
            ymin = float(self.ymin_control.manual_value())
        if self.ymax_control.is_auto():
            ymax = maxy
        else:
            ymax = float(self.ymax_control.manual_value())

        self.axes2.set_xbound(lower=xmin, upper=xmax)
        self.axes2.set_ybound(lower=ymin, upper=ymax)

    
     

    def on_update_update_button(self, event):
        label = "Update" if self.update else "Draw"
        self.update_button.SetLabel(label)

   
    def onKeyPress(self,event): 
        if event.GetKeyCode() == wx.WXK_RETURN:
            self.on_update_button(event)


    def boundary_layer(self):
        self.dpi3 = 102
        self.fig2 = Figure((3.0, 3.0), dpi=self.dpi3)
        self.axes3 = self.fig2.add_subplot(111)
        self.axes3.set_axis_bgcolor('white')
        self.x = arange(0,10,.5)
        self.y = arange(0,10,.5)
        self.X, self.Y = pylab.meshgrid(self.x,self.y)
        self.Z, self.Zt = pylab.meshgrid(self.x,self.y)

        if not self.update:
            self.axes3.contourf(self.X,self.Y,self.Z)

    def update_bl_mom(self,X,Y,U,maxk,jcur):
        Uinf = U[maxk]
        kct  = 0
        ucur = 0
        kmax = self.Bl_kplane.GetValue()
        while ( kct < kmax):
            ucur = U[kct]
            kct  = kct + 1
        delta = ( (X[kct] - X[0])**2 + (Y[kct] - Y[0])**2)**.5
        indmax = 0
        umax   = 0
        hitmax = True
        for i in range(0,maxk):
            if (U[i] > umax or i < 5):
                if hitmax:
                    umax = U[i]
                    indmax = i
            else:
                hitmax = False
#        print str(indmax)
        if indmax > maxk:
            indmax = maxk - 1
        if indmax%2 == 0:
            x = arange(0,(indmax/2),.5)
            y = arange(0,(indmax/2),.5)
            xpt = arange(0,(indmax/2),.5)
            ypt = arange(0,(indmax/2),.5)
        else:
            x = arange(0,(indmax/2)+.5,.5)
            y = arange(0,(indmax/2)+.5,.5)
            xpt = arange(0,(indmax/2)+.5,.5)
            ypt = arange(0,(indmax/2)+.5,.5)
        dispth = 0
        momth  = 0
        for i in range(0,indmax-1):
            uinf = U[indmax]
            y[i] = ( (X[i] - X[0])**2 + (Y[i] - Y[0])**2)**.5
            x[i] = U[i] / uinf
            xpt[i] = U[i] / uinf
            if i > 1:
                dy =  y[i] - y[i-1]
                dispth = dispth + (1 - xpt[i-1])*dy
                momth  = momth  +  xpt[i-1]*(1 - xpt[i-1])*dy
        H     = dispth/momth
#        print str(dispth)
#        print str(momth)
#        print str(H)
        shape = round(H,4)
        self.shape_label.SetLabel( ''.join(['H (Shape Factor) =   ', str(shape)]) )
        momthick = round(momth,8)
        self.mom_label.SetLabel( ''.join(['Momentum \nThickness =   ', str(momthick)]) )

        if (self.mom_norm.IsChecked()):
            self.axes3.cla()
            self.axes3.plot(x,(y/momth))
            self.axes3.set_xlabel("$ u/U_e $")
            self.axes3.set_ylabel("$ z/ \Theta$")
            self.axes3.set_xbound(lower=-.1, upper=1.4)
            self.axes3.set_ybound(lower=0, upper=15)
            self.canvas3.draw()
            self.fig2.subplots_adjust(wspace=0.1)
            self.fig2.subplots_adjust(left=0.2)
            self.fig2.subplots_adjust(right=0.95)
            self.fig2.subplots_adjust(bottom=0.15)


    def update_bl(self,X,Y,U,maxk,jcur):
        Uinf = U[maxk]
        kct  = 0
        ucur = 0
        kmax = self.Bl_kplane.GetValue()
        while ( kct < kmax):
            ucur = U[kct]
            kct  = kct + 1
        delta = ( (X[kct] - X[0])**2 + (Y[kct] - Y[0])**2)**.5
        x = [.5 for i in range(kct)]
        y = [.5 for i in range(kct)]
        xpt = [.5 for i in range(kct)]
        xpt = [.5 for i in range(kct)]
        for i in range(0,kct):
            y[i] = ( (X[i] - X[0])**2 + (Y[i] - Y[0])**2)**.5
            x[i] = U[i] / Uinf
        self.axes3.cla()
        self.axes3.plot(x,y)
        self.axes3.set_xlabel("$ u/U_e $")
        self.axes3.set_ylabel("$ z/c $")
        self.axes3.set_xbound(lower=-.1, upper=1.2)
        self.canvas3.draw() 
        self.fig2.subplots_adjust(wspace=0.1)
        self.fig2.subplots_adjust(left=0.28)
        self.fig2.subplots_adjust(right=0.95)
        self.fig2.subplots_adjust(bottom=0.15)
        
        self.line = self.axes2.plot(self.X[self.jcur,:],
                                     self.Y[self.jcur,:],
                                     color='b')
        self.check_auto_axis(-0.25,1.25,-0.25,0.25)
        self.canvas2.draw()
        # remove the most recent addition to the axes
        del self.axes2.lines[-1]
     
        


    def on_bl_button(self,event):
        self.selectq_control.SetString(string='Streamwise (u) Velocity')
        self.selectq_control.num = 2
        self.blcall = True
        self.bldraw = True
        self.on_update_button(event)
        self.blcall = False
        self.jcur = self.Bl_jplane.GetValue()
        gnum = self.qfile_control.manual_text2.GetValue()
        if (self.multgrd):
            new_str1 = ''.join([self.qfile, '\n','qvalbl.txt', '\n', str(gnum), '\n', str(self.jcur) ,' ',
                                str(self.jcur) ,' ', str(1), '\n' ,  str(1),' ',
                                str(self.nkind),' ',str(1), '\n' ,str(1) ,
                                ' ',str(1) ,' ', str(1), '\n',
                                str(2), '\n','n'])
        else:
            new_str1 = ''.join([self.qfile, '\n','qvalbl.txt', '\n', str(self.jcur) ,' ',
                                str(self.jcur) ,' ', str(1), '\n' ,  str(1),' ',
                                str(self.nkind),' ',str(1), '\n' ,str(1) ,
                                ' ',str(1) ,' ', str(1), '\n',
                                str(2), '\n','n'])

        new_str2 = ''.join([self.grid, '\n',  str(gnum), '\n', 'cordsbl.txt', '\n', str(self.jcur) ,' ',
                            str(self.jcur) ,' ', str(1), '\n' ,  str(1),' ',
                            str(self.nkind),' ', str(1), '\n' ,str(1) ,
                            ' ',str(1) ,' ', str(1)])
        p2 = subprocess.Popen(['getgridcords'], stdin=PIPE, stdout=PIPE)
        o2,e2 = p2.communicate(input=new_str2)
        p1 = subprocess.Popen(['listplotvar'], stdin=PIPE, stdout=PIPE)
        o1,e1 = p1.communicate(input=new_str1)
        p3 = subprocess.Popen(['tail', '-n', '+7'], stdin=PIPE, stdout=PIPE)
        o3,e3 = p3.communicate(input=o1)
        p4 = subprocess.Popen(['head', '-n', '-1'], stdin=PIPE, stdout=PIPE)
        o4,e4 = p4.communicate(input=o3)

        self.file1 = pylab.loadtxt('qvalbl.txt')
        self.file2 = pylab.loadtxt('cordsbl.txt')

        os.remove('qvalbl.txt')
        os.remove('cordsbl.txt')

        tot = len(self.file1)
        u = [.5 for i in range(tot)]
        x = [.5 for i in range(tot)]
        y = [.5 for i in range(tot)]
        for k in range(0,tot):
            u[k] = float(self.file1[k][3])
            x[k] = float(self.file2[k][3])
            y[k] = float(self.file2[k][4])
        xoverc = round(x[0],5)
        self.x_over_c.SetLabel( ''.join(['x/c =   ', str(xoverc)]) )
        if (self.mom_norm.IsChecked()):
            self.update_bl_mom(x,y,u,(self.nkind-1),(self.jcur))
        else:
            self.update_bl_mom(x,y,u,(self.nkind-1),(self.jcur))
            self.update_bl(x,y,u,(self.nkind-1),(self.jcur))



    def on_cb_stretch(self, event):
        self.on_update_button(event)

    def on_flatten(self, event):
        self.on_update_button(event)

    def on_cb_grid(self, event):
        self.show_grid()

    def on_mom_norm(self, event):
        self.on_bl_button(event)


    def on_save_plot(self, event):
        file_choices = "PNG (*.png)|*.png"

        dlg = wx.FileDialog(
            self,
            message="Save plot as...",
            defaultDir=os.getcwd(),
            defaultFile="plot.png",
            wildcard=file_choices,
            style=wx.SAVE)

        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            self.canvas2.print_figure(path, dpi=self.dpi2)
            self.flash_status_message("Saved to %s" % path)

    def on_redraw_timer(self, event):
        i = 1

    def on_exit(self, event):
        self.Destroy()

    def flash_status_message(self, msg, flash_len_ms=1500):
        self.statusbar.SetStatusText(msg)
        self.timeroff = wx.Timer(self)
        self.Bind(
            wx.EVT_TIMER,
            self.on_flash_status_off,
            self.timeroff)
        self.timeroff.Start(flash_len_ms, oneShot=True)

    def on_flash_status_off(self, event):
        self.statusbar.SetStatusText('')


if __name__ == '__main__':
    app = wx.PySimpleApp()
    app.frame = GraphFrame()
    app.frame.Show()
    app.MainLoop()



