"Display calendar in custom container. 
"Author: Przemyslaw Orpel
class zcl_calendar definition  public final.
  public section.
    methods:
      constructor importing ip_container type ref to cl_gui_custom_container,
      get_sel_dates exporting sdate type datum edate type datum change type abap_bool,
      set_change importing change type abap_bool.
  private section.
    data: gr_calendar  type ref to cl_gui_calendar,
          gv_nav_style type i,
          gv_sel_style type i,
          gv_change    type abap_bool.
endclass.



class zcl_calendar implementation.
* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CALENDAR->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_CONTAINER                   TYPE REF TO CL_GUI_CUSTOM_CONTAINER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.
    data: gv_today type cnca_utc_date.
    "Default focus today
    gv_today = sy-datum.

    "Set view and selection calendar style
    me->gv_nav_style = cnca_style_v_navigator + cnca_style_dtpicker + cl_gui_control=>ws_maximizebox.
    me->gv_sel_style =  cnca_sel_day + cnca_sel_week + cnca_sel_month + cnca_sel_interval.

    "Create calendar
    gr_calendar = new #( parent = ip_container
                         view_style = gv_nav_style
                         selection_style = gv_sel_style
                         focus_date = gv_today ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CALENDAR->GET_SEL_DATES
* +-------------------------------------------------------------------------------------------------+
* | [<---] SDATE                          TYPE        DATUM
* | [<---] EDATE                          TYPE        DATUM
* | [<---] CHANGE                         TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_sel_dates.
    data: lv_begin(8) type c,
          lv_end(8)   type c.

    "Get selected range/date
    gr_calendar->get_selection( importing date_begin = lv_begin date_end = lv_end ).
    sdate = lv_begin.
    edate = lv_end.

    "Information user closed calendar
    change = me->gv_change.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CALENDAR->SET_CHANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] CHANGE                         TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_change.
    me->gv_change = change.
  endmethod.
endclass.