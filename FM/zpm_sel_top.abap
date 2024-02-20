function-pool zpm_sel_top.                     "MESSAGE-ID ..
data: s100_ok      type sy-ucomm,
      gr_container type ref to cl_gui_custom_container,
      gr_calendar  type ref to zcl_calendar.
* INCLUDE LZPM_EQUID...                      " Local class definition
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'STATUS_100'. "Add 'CONFIRM' and 'CANCEL' to application toolbar
  set titlebar 'TITLE_100'. "Select date range
  if gr_calendar is initial.
    gr_container = new #( container_name =  'CONTAINER' ).
    gr_calendar = new #( ip_container = gr_container ).
  endif.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  case s100_ok.
    when 'CONFIRM'.
      gr_calendar->set_change( abap_true ).
      set screen 0.
    when 'CANCEL'.
      gr_calendar->set_change( abap_false ).
      set screen 0.
  endcase.
endmodule.