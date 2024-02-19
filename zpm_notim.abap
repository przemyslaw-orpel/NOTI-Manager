*&---------------------------------------------------------------------*
*& Report  ZPM_NOTIM
*&
*&---------------------------------------------------------------------*
*& Notification report GUI
*& Author: Przemyslaw Orpel
*&---------------------------------------------------------------------*
report zpm_notim.

data: s100_ok   type syst_ucomm. "screen 100 element

class lc_gui_screen definition final.
  public section.
    class-data:
    screen type ref to lc_gui_screen. "Singleton
    class-methods:
      create_screen.
    methods:
      constructor.
  private section.
    methods:
      create_splitter,
      set_icon,
      create_tree,
      select_tplnr_data,
      init_tree,
      create_nodes,
      select_equi_data,
      add_equi_nodes,
      set_tree_col_settings,
      hide_column_tree,
      set_tree_handler,
      set_tree_toolbar,
      create_alv_noti,
      create_alv_release,
      create_alv_open,
      create_alv_closed,
      set_alv_toolbar,
      set_alv_header,
      set_alv_hotspot importing ip_alv type ref to cl_salv_table,
      set_alv_handler,
      select_noti importing ip_ls_tree type zpm_tree ip_noti_status type string exporting ep_lt_noti type zpm_noti_tab,
      display_alv_noti,
      refresh_alv_noti,
      on_link_click importing ip_column type salv_de_column ip_row type salv_de_row ip_table type zpm_noti_tab,
      on_tree_click for event double_click of cl_salv_events_tree importing node_key,
      on_link_open for event link_click of cl_salv_events_table importing column row,
      on_link_relase for event link_click of cl_salv_events_table importing column row,
      on_link_closed for event link_click of cl_salv_events_table importing column row.

    constants: gc_noti_open    type string value 'I0068',
               gc_noti_release type string value 'I0070',
               gc_noti_closed  type string value 'I0072',
               gc_qmnum        type lvc_fname value 'QMNUM',
               gc_aufnr        type lvc_fname value 'AUFNR',
               gc_error        type c length 1 value 'E'.

    types:    begin of ty_tplnr_key,
                tplma    type tplma,
                node_key type i,
              end of ty_tplnr_key,
              begin of ty_equi_key,
                equnr    type equnr,
                node_key type i,
              end of ty_equi_key.

    data:
      gr_tree_cont     type ref to cl_gui_container,
      gr_top_cont      type ref to cl_gui_container,
      gr_center_cont   type ref to cl_gui_container,
      gr_bottom_cont   type ref to cl_gui_container,
      gr_noti_splitter type ref to cl_gui_splitter_container,
      gr_splitter      type ref to cl_gui_splitter_container,
      gr_alv_tree      type ref to cl_salv_tree,
      gr_alv_release   type ref to cl_salv_table,
      gr_alv_open      type ref to cl_salv_table,
      gr_alv_closed    type ref to cl_salv_table,
      gt_tree_init     type zpm_tree_tab,
      gt_tree_tplnr    type zpm_tree_tab,
      gt_tree_equi     type zpm_tree_tab,
      gt_tplnr_key     type table of  ty_tplnr_key,
      gt_equi_key      type table of  ty_equi_key,
      gt_noti_release  type zpm_noti_tab,
      gt_noti_open     type zpm_noti_tab,
      gt_noti_closed   type zpm_noti_tab,
      gv_hier_icon     type salv_de_tree_image,
      gv_expand_icon   type salv_de_tree_image,
      gv_equi_icon     type salv_de_tree_image,
      gv_collapse_icon type salv_de_tree_image.
endclass.

class lc_gui_screen implementation.
  method constructor.
    me->create_splitter( ).
    me->set_icon( ).
    me->create_tree( ).
    me->create_alv_noti( ).
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
    gr_splitter->set_column_width( id = 1 width = 30 ).

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
  method set_icon.
    " Set icon
    gv_expand_icon = icon_expand_all.
    gv_collapse_icon = icon_collapse_all.
    gv_equi_icon = icon_equipment .

  endmethod.
  method create_tree.
    me->init_tree( ).
    me->select_tplnr_data( ).
    me->create_nodes( ).
    me->select_equi_data( ).
    me->add_equi_nodes( ).
    me->set_tree_col_settings( ).
    me->hide_column_tree( ).
    me->set_tree_handler( ).
    me->set_tree_toolbar( ).
    gr_alv_tree->display( ).
  endmethod.
  method select_tplnr_data.
    data: lt_iflot    type table of iflot,
          lv_node_key type i,
          lv_parent   type i.

    " Select data from DB
    select distinct * from iflot
      into table lt_iflot order by tplma tplnr.

    "Set node key to tplnr
    loop at lt_iflot into data(lv_tplma).
      append value ty_tplnr_key( tplma = lv_tplma-tplnr
                                 node_key = sy-tabix ) to gt_tplnr_key.
    endloop.

    "Set node_key and parent for tplnr
    loop at lt_iflot into data(lv_iflot).
      lv_node_key = 0.
      lv_parent = 0.
      "Find node key (tplnr)
      read table gt_tplnr_key into data(wa_tplnr_key)
         with table key tplma = lv_iflot-tplnr.
      if sy-subrc eq 0.
        lv_parent = wa_tplnr_key-node_key.
      endif.

      "Find parent key (tplma)
      read table gt_tplnr_key into wa_tplnr_key
         with table key tplma = lv_iflot-tplma.
      if sy-subrc eq 0.
        lv_node_key = wa_tplnr_key-node_key.
      endif.

      " Add row to tree table
      append value zpm_tree(
          node_key    =  lv_parent
          node_parent = lv_node_key
          tplnr       = lv_iflot-tplnr ) to gt_tree_tplnr .
    endloop.

    " Fill short texts (pltxt)
    loop at gt_tree_tplnr   assigning field-symbol(<fs_tree_tab>).
      select single pltxt from iflotx
        into <fs_tree_tab>-eqktx
        where tplnr = <fs_tree_tab>-tplnr.
    endloop.

    " Important! Sort tree tabel by node_key and parent
    sort gt_tree_tplnr  by node_key node_parent.
  endmethod.
  method init_tree.
    "Create ALV Tree instance
    try.
        cl_salv_tree=>factory(
          exporting
            r_container = gr_tree_cont
          importing
            r_salv_tree = gr_alv_tree
           changing
            t_table = gt_tree_init ). " must be empty
      catch cx_salv_error into data(lx_alv_error).
        message lx_alv_error->get_text( ) type gc_error.
    endtry.
  endmethod.
  method create_nodes.
    " Get nodes from alv tree
    data(lr_nodes) = gr_alv_tree->get_nodes( ).

    "Filling the tree
    try.
        loop at gt_tree_tplnr  assigning field-symbol(<fs_tree>).
          if <fs_tree>-node_parent eq 0.
            "Add first nodes
            lr_nodes->add_node(
                related_node = ''
                relationship = cl_gui_column_tree=>relat_last_child
                collapsed_icon = gv_expand_icon
                data_row = <fs_tree>
                row_style = if_salv_c_tree_style=>emphasized_a
                text = | { <fs_tree>-tplnr } | ).
            continue.
          else.
            lr_nodes->add_node(
                related_node = conv #( <fs_tree>-node_parent )
                relationship   = cl_gui_column_tree=>relat_last_child
                collapsed_icon = gv_expand_icon
                expanded_icon  = gv_collapse_icon
                data_row       = <fs_tree>
                row_style      = if_salv_c_tree_style=>emphasized_a
                text           = | { <fs_tree>-tplnr }| ).
          endif.
        endloop.
      catch cx_salv_msg into data(lx_alv_error).
        message lx_alv_error->get_text( ) type gc_error.
    endtry.
  endmethod.
  method select_equi_data.
    data: lv_node_key type i,
          lv_parent   type i.
    " Select equimpent from DB
    select distinct * from equi
      join eqkt on equi~equnr = eqkt~equnr
      join equz on equi~equnr = equz~equnr
      join iloa on equz~iloan = iloa~iloan
      join iflot on iloa~tplnr = iflot~tplnr
      join iflotx on iloa~tplnr = iflotx~tplnr
      into corresponding fields of table @gt_tree_equi
      where equz~datbi gt @sy-datum.
    "and iflot~tplnr like ''  - limit equipments for the tplnr

    " Read last key index
    data(lv_key_index) = lines( gt_tplnr_key ) .

    " Set equi key without hequi
    loop at gt_tree_equi assigning field-symbol(<fs_tree_equnr>).
      if <fs_tree_equnr>-hequi <> ''. continue. endif.
      lv_key_index = lv_key_index + 1.
      append value ty_equi_key( equnr =  <fs_tree_equnr>-equnr node_key = lv_key_index ) to gt_equi_key.
    endloop.

    " Set equi key for equimpent sub equipment
    do 5 times. "In my case equimpent may have 5 sub equipment
      loop at gt_tree_equi into <fs_tree_equnr>.
        if <fs_tree_equnr>-hequi eq ''. continue. endif.
        read table gt_equi_key into data(ls_equi_key) with key equnr = <fs_tree_equnr>-hequi.
        if sy-subrc eq 0.
          read table gt_equi_key into ls_equi_key with key equnr = <fs_tree_equnr>-equnr.
          if sy-subrc <> 0.
            lv_key_index = lv_key_index + 1.
            append value ty_equi_key( equnr =  <fs_tree_equnr>-equnr node_key = lv_key_index ) to gt_equi_key.
          endif.
        endif.
      endloop.
    enddo.

    "Set node_key and parent for equimpent
    loop at gt_tree_equi assigning field-symbol(<fs_tree>).
      lv_node_key = 0.
      lv_parent = 0.

      if <fs_tree>-hequi <> ''.
        "Find node key (equi)
        read table gt_equi_key into data(ls_equnr_key)
           with table key equnr = <fs_tree>-equnr.
        if sy-subrc eq 0.
          lv_node_key = ls_equnr_key-node_key.
        endif.
        "Find parent key (hequi)
        read table gt_equi_key into ls_equnr_key
           with table key equnr = <fs_tree>-hequi.
        if sy-subrc eq 0.
          lv_parent = ls_equnr_key-node_key.
        endif.
      else.
        "Find node key (equi)
        read table gt_equi_key into ls_equnr_key
           with table key equnr = <fs_tree>-equnr.
        if sy-subrc eq 0.
          lv_node_key = ls_equnr_key-node_key.
        endif.
        "Find parent key (tplnr)
        read table gt_tplnr_key into data(ls_tplnr_key)
           with table key tplma = <fs_tree>-tplnr.
        if sy-subrc eq 0.
          lv_parent = ls_tplnr_key-node_key.
        endif.
      endif.
      <fs_tree>-node_key = lv_node_key.
      <fs_tree>-node_parent = lv_parent.
    endloop.
    " Important! Sort tree tabel by node_key and parent
    sort gt_tree_equi  by node_key node_parent.
  endmethod.
  method add_equi_nodes.
    data:  lv_text     type lvc_value.
    " Get nodes from alv tree
    data(lr_nodes) = gr_alv_tree->get_nodes( ).
    "Filling the tree
    try.
        loop at gt_tree_equi  assigning field-symbol(<fs_equi>).
          lv_text = <fs_equi>-equnr.
          shift lv_text left deleting leading '0'.
          lr_nodes->add_node(
              related_node = conv #( <fs_equi>-node_parent )
              relationship   = cl_gui_column_tree=>relat_last_child
              collapsed_icon = gv_equi_icon
              expanded_icon  = gv_equi_icon
              data_row       = <fs_equi>
              row_style      = if_salv_c_tree_style=>emphasized_positive
              text           = lv_text ).
        endloop.
      catch cx_salv_msg into data(lx_alv_error).
        message lx_alv_error->get_text( ) type gc_error.
    endtry.
  endmethod.
  method set_tree_col_settings.
    data(lr_setting) = gr_alv_tree->get_tree_settings( ).
    lr_setting->set_hierarchy_header( text-004 ). " Fun. location / Equipment
    lr_setting->set_hierarchy_size( 50 ).
    lr_setting->set_hierarchy_icon( gv_hier_icon ).
  endmethod.
  method hide_column_tree.
    data: lt_hide_col type table of lvc_fname.
    lt_hide_col = value #( ( 'MANDT' ) ( 'NODE_KEY' ) ( 'NODE_PARENT' ) ( 'TPLNR' ) ( 'TPLMA' )
     ( 'EQUNR' ) ( 'PLTXT' ) ( 'HEQUI' ) ).

    try.
        data(lo_tree_col) = gr_alv_tree->get_columns( ).
        loop at lt_hide_col into data(ls_col).
          lo_tree_col->get_column( ls_col )->set_visible( abap_false ).
        endloop.
      catch cx_salv_not_found into data(lx_alv_error).
        message lx_alv_error->get_text( ) type  gc_error.
    endtry.
  endmethod.
  method  set_tree_handler.
    "Set tree handler
    data(lo_event) = gr_alv_tree->get_event( ).
    set handler me->on_tree_click for lo_event.
  endmethod.
  method set_tree_toolbar.
    me->gr_alv_tree->get_functions( )->set_all( ).
  endmethod.
  method create_alv_noti.
    me->create_alv_release( ).
    me->create_alv_open( ).
    me->create_alv_closed( ).
    me->set_alv_toolbar( ).
    me->set_alv_header( ).
    me->set_alv_hotspot( gr_alv_open ).
    me->set_alv_hotspot( gr_alv_release ).
    me->set_alv_hotspot( gr_alv_closed ).
    me->set_alv_handler( ).

    me->display_alv_noti( ).
  endmethod.
  method create_alv_release.
    "Create ALV Table instance
    try.
        cl_salv_table=>factory(
          exporting
            r_container = gr_center_cont
          importing
            r_salv_table = gr_alv_release
          changing
            t_table = gt_noti_release
        ).
      catch cx_salv_msg into data(lx_error).
        message lx_error->get_text( ) type  gc_error.
    endtry.
  endmethod.
  method create_alv_open.
    "Create ALV Table instance
    try.
        cl_salv_table=>factory(
          exporting
            r_container = gr_top_cont
          importing
            r_salv_table = gr_alv_open
          changing
            t_table = gt_noti_open
        ).
      catch cx_salv_msg into data(lx_error).
        message lx_error->get_text( ) type gc_error.
    endtry.
  endmethod.
  method create_alv_closed.
    "Create ALV Table instance
    try.
        cl_salv_table=>factory(
          exporting
            r_container = gr_bottom_cont
          importing
            r_salv_table = gr_alv_closed
          changing
            t_table = gt_noti_closed
        ).
      catch cx_salv_msg into data(lx_error).
        message lx_error->get_text( ) type gc_error.
    endtry.
  endmethod.
  method set_alv_toolbar.
    me->gr_alv_open->get_functions( )->set_all( ).
    me->gr_alv_release->get_functions( )->set_all( ).
    me->gr_alv_closed->get_functions( )->set_all( ).
  endmethod.
  method set_alv_header.
    me->gr_alv_open->get_display_settings( )->set_list_header( text-001 ). "Notification open
    me->gr_alv_release->get_display_settings( )->set_list_header( text-002 ). "Notification release
    me->gr_alv_closed->get_display_settings( )->set_list_header( text-003 ). "Notification closed
  endmethod.
  method set_alv_hotspot.
    "Set column hotspot
    try.
        data(lr_qmnum_col) = cast cl_salv_column_table(
          ip_alv->get_columns( )->get_column( gc_qmnum ) ).
        lr_qmnum_col->set_cell_type( if_salv_c_cell_type=>hotspot ).

        data(lr_aufnr_col) = cast cl_salv_column_table(
          ip_alv->get_columns( )->get_column( gc_aufnr ) ).
        lr_aufnr_col->set_cell_type( if_salv_c_cell_type=>hotspot ).
      catch cx_salv_not_found into data(lx_column_error).
        message lx_column_error->get_text( ) type gc_error.
    endtry.
  endmethod.
  method set_alv_handler.
    " Register event for each alv
    data(lr_event) = gr_alv_open->get_event( ).
    set handler me->on_link_open for lr_event.

    lr_event = gr_alv_release->get_event( ).
    set handler me->on_link_relase for lr_event.

    lr_event = gr_alv_closed->get_event( ).
    set handler me->on_link_closed for lr_event.
  endmethod.
  method display_alv_noti.
    "Display alv
    me->gr_alv_release->display( ).
    me->gr_alv_open->display( ).
    me->gr_alv_closed->display( ).
  endmethod.
  method on_tree_click.
    "Combine gt_tree_tplnr and gt_tree_equi to lt_tree_total to read selected line
    data: lt_tree_total type zpm_tree_tab.
    append lines of gt_tree_tplnr to lt_tree_total.
    append lines of gt_tree_equi to lt_tree_total.

    "Read selected row
    read table lt_tree_total into data(ls_tree) with table key node_key = node_key.

    "Find notification by system status
    me->select_noti( exporting ip_ls_tree = ls_tree ip_noti_status = gc_noti_open importing ep_lt_noti = gt_noti_open ).
    me->select_noti( exporting ip_ls_tree = ls_tree ip_noti_status = gc_noti_release importing ep_lt_noti = gt_noti_release ).
    me->select_noti( exporting ip_ls_tree = ls_tree ip_noti_status = gc_noti_closed importing ep_lt_noti = gt_noti_closed ).

    "Refresh alv notification
    me->refresh_alv_noti( ).
  endmethod.
  method on_link_open.
    me->on_link_click( ip_column = column ip_row = row ip_table = gt_noti_open ).
  endmethod.
  method on_link_relase.
    me->on_link_click( ip_column = column ip_row = row ip_table = gt_noti_release ).
  endmethod.
  method on_link_closed.
    me->on_link_click( ip_column = column ip_row = row ip_table = gt_noti_closed ).
  endmethod.
  method on_link_click.
    read table ip_table into data(ls_noti) index ip_row.
    "Open IW23 or IW33 transaction
    case ip_column.
      when gc_qmnum.
        set parameter id: 'IQM' field ls_noti-qmnum.
        call transaction 'IW23' and skip first screen.
      when gc_aufnr.
        set parameter id: 'ANR' field ls_noti-aufnr.
        call transaction 'IW33' and skip first screen.
    endcase.
  endmethod.

  method refresh_alv_noti.
    me->gr_alv_release->refresh( ).
    me->gr_alv_open->refresh( ).
    me->gr_alv_closed->refresh( ).
  endmethod.

  method select_noti.
    "Check selected row is fun. location or equipment
    if ip_ls_tree-equnr eq ''.
      select * from qmih
        join iloa on qmih~iloan = iloa~iloan
        join qmel on qmih~qmnum = qmel~qmnum
        join jest on qmel~objnr = jest~objnr
       into corresponding fields of table @ep_lt_noti
       where iloa~tplnr = @ip_ls_tree-tplnr and
             jest~stat eq @ip_noti_status and jest~inact = @abap_false
       order by qmel~erdat, qmel~mzeit.
    else.
      select * from qmih
         join iloa on qmih~iloan = iloa~iloan
         join qmel on qmih~qmnum = qmel~qmnum
         join jest on qmel~objnr = jest~objnr
        into corresponding fields of table @ep_lt_noti
        where qmih~equnr = @ip_ls_tree-equnr and
              jest~stat eq @ip_noti_status and jest~inact = @abap_false
        order by qmel~erdat, qmel~mzeit.
    endif.
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
  set titlebar 'TITLE_100'. "Notification report gui
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