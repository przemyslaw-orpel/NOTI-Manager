*&---------------------------------------------------------------------*
*& Report  ZPM_NOTIM
*&
*&---------------------------------------------------------------------*
*& Noficiation managment GUI
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
      create_splitter,
      set_icon,
      create_tree,
      select_tplnr_data,
      init_tree,
      create_nodes,
      select_equi_data,
      add_equi_nodes,
      set_tree_col_settings,
      hide_column_tree.

    types: begin of ty_tplnr_key,
             tplma    type tplma,
             node_key type i,
           end of ty_tplnr_key,
           begin of ty_equi_key,
             equnr    type equnr,
             node_key type i,
           end of ty_equi_key,
           begin of ty_tree,
             node_key    type i,  "tplnr key
             node_parent type i,  "tplma key
             tplnr       type tplnr,
             tplma       type tplma,
             pltxt       type pltxt,
             equnr       type equnr,
             hequi       type hequi,
             eqktx       type ktx01,
           end of ty_tree.

    data:
      gr_tree_cont     type ref to cl_gui_container,
      gr_top_cont      type ref to cl_gui_container,
      gr_center_cont   type ref to cl_gui_container,
      gr_bottom_cont   type ref to cl_gui_container,
      gr_noti_splitter type ref to cl_gui_splitter_container,
      gr_splitter      type ref to cl_gui_splitter_container,
      gr_alv_tree      type ref to cl_salv_tree,
      gt_tree_init     type table of ty_tree,
      gt_tree_tplnr    type table of ty_tree,
      gt_tree_equi     type table of ty_tree,
      gt_tplnr_key     type table of  ty_tplnr_key,
      gt_equi_key      type table of  ty_equi_key,
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
      append value ty_tree(
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
        message lx_alv_error->get_text( ) type 'E'.
    endtry.
  endmethod.
  method create_nodes.
    data: lv_text type lvc_value.
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
        message lx_alv_error->get_text( ) type 'E'.
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
        message lx_alv_error->get_text( ) type 'E'.
    endtry.
  endmethod.
  method set_tree_col_settings.
    data(lr_setting) = gr_alv_tree->get_tree_settings( ).
    lr_setting->set_hierarchy_header( 'TPLNR TREE' ).
    lr_setting->set_hierarchy_size( 50 ).
    lr_setting->set_hierarchy_icon( gv_hier_icon ).
  endmethod.
  method hide_column_tree.
    data: lt_hide_col type table of lvc_fname.
    lt_hide_col = value #( ( 'NODE_KEY' ) ( 'NODE_PARENT' ) ( 'TPLNR' ) ( 'TPLMA' )
     ( 'EQUNR' ) ( 'PLTXT' ) ( 'HEQUI' ) ).

    try.
        data(lo_tree_col) = gr_alv_tree->get_columns( ).
        loop at lt_hide_col into data(ls_col).
          lo_tree_col->get_column( ls_col )->set_visible( abap_false ).
        endloop.
      catch cx_salv_not_found into data(lx_alv_error).
        message lx_alv_error->get_text( ) type 'E'.
    endtry.
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