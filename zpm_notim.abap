*&---------------------------------------------------------------------*
*& Report  ZPM_NOTIM
*&
*&---------------------------------------------------------------------*
*& Notification managment GUI
*& Author: Przemyslaw Orpel
*&---------------------------------------------------------------------*
report zpm_notim.

data: s100_ok   type syst_ucomm. "screen 100 element


class lc_gui_screen definition.
  public section.
    class-data:
    screen type ref to lc_gui_screen. "Singleton
    class-methods:
      create_screen.
    methods:
      constructor.
  private section.
    methods:
      create_splitter.
    data:
      gr_tree_cont     type ref to cl_gui_container,
      gr_top_cont      type ref to cl_gui_container,
      gr_center_cont   type ref to cl_gui_container,
      gr_bottom_cont   type ref to cl_gui_container,
      gr_noti_splitter type ref to cl_gui_splitter_container,
      gr_splitter      type ref to cl_gui_splitter_container.

endclass.

class lc_gui_screen implementation.
  method constructor.
    me->create_splitter( ).
  endmethod.
  method create_screen.
    if screen is initial.
      screen = new #( ).
    endif.
  endmethod.
  method create_splitter.
    " Create horizontal splitter instance
    gr_splitter = new #( parent = cl_gui_container=>default_screen
                         rows = 1
                         columns = 2 ).
    " Set first column width
    gr_splitter->set_column_width( id = 1 width = 20 ).

    " Create vertical splitter instance
    gr_noti_splitter = new #( parent =   gr_splitter->get_container( row = 1 column = 2 )
                           rows = 3
                           columns = 1 ).
    " Set containers
    gr_tree_cont = gr_splitter->get_container( row = 1 column = 1  ).
    gr_top_cont = gr_noti_splitter->get_container( row = 1 column = 1 ).
    gr_center_cont = gr_noti_splitter->get_container( row = 2 column = 1 ).
    gr_bottom_cont = gr_noti_splitter->get_container( row = 3 column = 1 ).
  endmethod.
endclass.

start-of-selection.
  call screen 100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       PBO
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'STATUS_100'.
  set titlebar 'TITLE_100'. "Notification management
  lc_gui_screen=>create_screen( ).
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       PAI
*----------------------------------------------------------------------*
module user_command_0100 input.
  case s100_ok.
    when 'EXIT'.
      leave program.
  endcase.
endmodule.