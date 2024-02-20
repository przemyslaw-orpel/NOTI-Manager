function zsel_date.
*"----------------------------------------------------------------------
*"*"Lokalny interfejs:
*"  EXPORTING
*"     REFERENCE(E_SDATE) TYPE  DATUM
*"     REFERENCE(E_EDATE) TYPE  DATUM
*"     REFERENCE(E_CHANGE) TYPE  ABAP_BOOL
*"----------------------------------------------------------------------

  call screen 100  starting at  40  3.
  gr_calendar->get_sel_dates( importing sdate = e_sdate edate = e_edate change = e_change ).
endfunction.