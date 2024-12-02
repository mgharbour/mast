pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__gmast_pt_editor.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__gmast_pt_editor.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E069 : Short_Integer; pragma Import (Ada, E069, "system__os_lib_E");
   E016 : Short_Integer; pragma Import (Ada, E016, "ada__exceptions_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "system__soft_links_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "system__exception_table_E");
   E035 : Short_Integer; pragma Import (Ada, E035, "ada__containers_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "ada__io_exceptions_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "ada__numerics_E");
   E007 : Short_Integer; pragma Import (Ada, E007, "ada__strings_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__strings__maps_E");
   E056 : Short_Integer; pragma Import (Ada, E056, "ada__strings__maps__constants_E");
   E040 : Short_Integer; pragma Import (Ada, E040, "interfaces__c_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__exceptions_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "system__object_reader_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "system__dwarf_lines_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "system__soft_links__initialize_E");
   E034 : Short_Integer; pragma Import (Ada, E034, "system__traceback__symbolic_E");
   E379 : Short_Integer; pragma Import (Ada, E379, "ada__assertions_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "ada__strings__utf_encoding_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "ada__tags_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "ada__strings__text_buffers_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "gnat_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "interfaces__c__strings_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "ada__streams_E");
   E415 : Short_Integer; pragma Import (Ada, E415, "system__file_control_block_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "system__finalization_root_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "ada__finalization_E");
   E414 : Short_Integer; pragma Import (Ada, E414, "system__file_io_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "system__storage_pools_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "system__finalization_masters_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "system__storage_pools__subpools_E");
   E451 : Short_Integer; pragma Import (Ada, E451, "ada__strings__unbounded_E");
   E461 : Short_Integer; pragma Import (Ada, E461, "ada__calendar_E");
   E410 : Short_Integer; pragma Import (Ada, E410, "ada__text_io_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "system__pool_global_E");
   E668 : Short_Integer; pragma Import (Ada, E668, "system__random_seed_E");
   E475 : Short_Integer; pragma Import (Ada, E475, "system__regexp_E");
   E465 : Short_Integer; pragma Import (Ada, E465, "ada__directories_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "glib_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "gtkada__types_E");
   E554 : Short_Integer; pragma Import (Ada, E554, "binary_trees_E");
   E477 : Short_Integer; pragma Import (Ada, E477, "changes_control_E");
   E479 : Short_Integer; pragma Import (Ada, E479, "check_operations_E");
   E226 : Short_Integer; pragma Import (Ada, E226, "gdk__frame_timings_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "glib__glist_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "gdk__visual_E");
   E175 : Short_Integer; pragma Import (Ada, E175, "glib__gslist_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "gtkada__c_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "glib__object_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "glib__type_conversion_hooks_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "glib__types_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "glib__values_E");
   E161 : Short_Integer; pragma Import (Ada, E161, "gtkada__bindings_E");
   E182 : Short_Integer; pragma Import (Ada, E182, "cairo_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "cairo__region_E");
   E201 : Short_Integer; pragma Import (Ada, E201, "gdk__rectangle_E");
   E189 : Short_Integer; pragma Import (Ada, E189, "glib__generic_properties_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "gdk__color_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "gdk__rgba_E");
   E337 : Short_Integer; pragma Import (Ada, E337, "glib__key_file_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "glib__properties_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "gdk__device_tool_E");
   E291 : Short_Integer; pragma Import (Ada, E291, "gdk__drawing_context_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "gdk__event_E");
   E236 : Short_Integer; pragma Import (Ada, E236, "glib__string_E");
   E234 : Short_Integer; pragma Import (Ada, E234, "glib__variant_E");
   E299 : Short_Integer; pragma Import (Ada, E299, "glib__g_icon_E");
   E371 : Short_Integer; pragma Import (Ada, E371, "gtk__actionable_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "gtk__builder_E");
   E279 : Short_Integer; pragma Import (Ada, E279, "gtk__buildable_E");
   E311 : Short_Integer; pragma Import (Ada, E311, "gtk__cell_area_context_E");
   E327 : Short_Integer; pragma Import (Ada, E327, "gtk__css_section_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "gtk__enums_E");
   E526 : Short_Integer; pragma Import (Ada, E526, "gtk__file_filter_E");
   E285 : Short_Integer; pragma Import (Ada, E285, "gtk__orientable_E");
   E339 : Short_Integer; pragma Import (Ada, E339, "gtk__paper_size_E");
   E335 : Short_Integer; pragma Import (Ada, E335, "gtk__page_setup_E");
   E345 : Short_Integer; pragma Import (Ada, E345, "gtk__print_settings_E");
   E530 : Short_Integer; pragma Import (Ada, E530, "gtk__stock_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "gtk__target_entry_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "gtk__target_list_E");
   E365 : Short_Integer; pragma Import (Ada, E365, "gtk__text_mark_E");
   E560 : Short_Integer; pragma Import (Ada, E560, "list_exceptions_E");
   E649 : Short_Integer; pragma Import (Ada, E649, "doubly_linked_lists_E");
   E683 : Short_Integer; pragma Import (Ada, E683, "dynamic_lists_E");
   E689 : Short_Integer; pragma Import (Ada, E689, "associations_E");
   E589 : Short_Integer; pragma Import (Ada, E589, "hash_lists_E");
   E581 : Short_Integer; pragma Import (Ada, E581, "indexed_lists_E");
   E253 : Short_Integer; pragma Import (Ada, E253, "pango__enums_E");
   E273 : Short_Integer; pragma Import (Ada, E273, "pango__attributes_E");
   E257 : Short_Integer; pragma Import (Ada, E257, "pango__font_metrics_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "pango__language_E");
   E255 : Short_Integer; pragma Import (Ada, E255, "pango__font_E");
   E351 : Short_Integer; pragma Import (Ada, E351, "gtk__text_attributes_E");
   E353 : Short_Integer; pragma Import (Ada, E353, "gtk__text_tag_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "pango__font_face_E");
   E261 : Short_Integer; pragma Import (Ada, E261, "pango__font_family_E");
   E265 : Short_Integer; pragma Import (Ada, E265, "pango__fontset_E");
   E267 : Short_Integer; pragma Import (Ada, E267, "pango__matrix_E");
   E251 : Short_Integer; pragma Import (Ada, E251, "pango__context_E");
   E269 : Short_Integer; pragma Import (Ada, E269, "pango__font_map_E");
   E275 : Short_Integer; pragma Import (Ada, E275, "pango__tabs_E");
   E271 : Short_Integer; pragma Import (Ada, E271, "pango__layout_E");
   E341 : Short_Integer; pragma Import (Ada, E341, "gtk__print_context_E");
   E224 : Short_Integer; pragma Import (Ada, E224, "gdk__frame_clock_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "gdk__monitor_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "gdk__display_E");
   E293 : Short_Integer; pragma Import (Ada, E293, "gdk__glcontext_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "gdk__pixbuf_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "gdk__screen_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "gdk__device_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "gdk__drag_contexts_E");
   E289 : Short_Integer; pragma Import (Ada, E289, "gdk__window_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "glib__action_group_E");
   E238 : Short_Integer; pragma Import (Ada, E238, "gtk__accel_group_E");
   E283 : Short_Integer; pragma Import (Ada, E283, "gtk__adjustment_E");
   E301 : Short_Integer; pragma Import (Ada, E301, "gtk__cell_editable_E");
   E303 : Short_Integer; pragma Import (Ada, E303, "gtk__editable_E");
   E305 : Short_Integer; pragma Import (Ada, E305, "gtk__entry_buffer_E");
   E323 : Short_Integer; pragma Import (Ada, E323, "gtk__icon_source_E");
   E343 : Short_Integer; pragma Import (Ada, E343, "gtk__print_operation_preview_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "gtk__selection_data_E");
   E361 : Short_Integer; pragma Import (Ada, E361, "gtk__clipboard_E");
   E244 : Short_Integer; pragma Import (Ada, E244, "gtk__style_E");
   E357 : Short_Integer; pragma Import (Ada, E357, "gtk__scrollable_E");
   E349 : Short_Integer; pragma Import (Ada, E349, "gtk__text_iter_E");
   E367 : Short_Integer; pragma Import (Ada, E367, "gtk__text_tag_table_E");
   E317 : Short_Integer; pragma Import (Ada, E317, "gtk__tree_model_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "gtk__widget_E");
   E315 : Short_Integer; pragma Import (Ada, E315, "gtk__cell_renderer_E");
   E313 : Short_Integer; pragma Import (Ada, E313, "gtk__cell_layout_E");
   E309 : Short_Integer; pragma Import (Ada, E309, "gtk__cell_area_E");
   E281 : Short_Integer; pragma Import (Ada, E281, "gtk__container_E");
   E295 : Short_Integer; pragma Import (Ada, E295, "gtk__bin_E");
   E277 : Short_Integer; pragma Import (Ada, E277, "gtk__box_E");
   E307 : Short_Integer; pragma Import (Ada, E307, "gtk__entry_completion_E");
   E329 : Short_Integer; pragma Import (Ada, E329, "gtk__misc_E");
   E331 : Short_Integer; pragma Import (Ada, E331, "gtk__notebook_E");
   E347 : Short_Integer; pragma Import (Ada, E347, "gtk__status_bar_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "gtk__style_provider_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "gtk__settings_E");
   E325 : Short_Integer; pragma Import (Ada, E325, "gtk__style_context_E");
   E321 : Short_Integer; pragma Import (Ada, E321, "gtk__icon_set_E");
   E319 : Short_Integer; pragma Import (Ada, E319, "gtk__image_E");
   E297 : Short_Integer; pragma Import (Ada, E297, "gtk__gentry_E");
   E363 : Short_Integer; pragma Import (Ada, E363, "gtk__text_child_anchor_E");
   E359 : Short_Integer; pragma Import (Ada, E359, "gtk__text_buffer_E");
   E355 : Short_Integer; pragma Import (Ada, E355, "gtk__text_view_E");
   E287 : Short_Integer; pragma Import (Ada, E287, "gtk__window_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "gtk__dialog_E");
   E333 : Short_Integer; pragma Import (Ada, E333, "gtk__print_operation_E");
   E180 : Short_Integer; pragma Import (Ada, E180, "gtk__arguments_E");
   E433 : Short_Integer; pragma Import (Ada, E433, "glib__menu_model_E");
   E711 : Short_Integer; pragma Import (Ada, E711, "gtk__about_dialog_E");
   E369 : Short_Integer; pragma Import (Ada, E369, "gtk__action_E");
   E373 : Short_Integer; pragma Import (Ada, E373, "gtk__activatable_E");
   E492 : Short_Integer; pragma Import (Ada, E492, "gtk__alignment_E");
   E178 : Short_Integer; pragma Import (Ada, E178, "gtk__button_E");
   E417 : Short_Integer; pragma Import (Ada, E417, "gtk__cell_renderer_text_E");
   E524 : Short_Integer; pragma Import (Ada, E524, "gtk__file_chooser_E");
   E528 : Short_Integer; pragma Import (Ada, E528, "gtk__file_chooser_dialog_E");
   E419 : Short_Integer; pragma Import (Ada, E419, "gtk__frame_E");
   E705 : Short_Integer; pragma Import (Ada, E705, "gtk__grange_E");
   E571 : Short_Integer; pragma Import (Ada, E571, "gtk__main_E");
   E381 : Short_Integer; pragma Import (Ada, E381, "gtk__marshallers_E");
   E387 : Short_Integer; pragma Import (Ada, E387, "gtk__menu_item_E");
   E385 : Short_Integer; pragma Import (Ada, E385, "gtk__image_menu_item_E");
   E435 : Short_Integer; pragma Import (Ada, E435, "gtk__menu_shell_E");
   E431 : Short_Integer; pragma Import (Ada, E431, "gtk__menu_E");
   E429 : Short_Integer; pragma Import (Ada, E429, "gtk__label_E");
   E699 : Short_Integer; pragma Import (Ada, E699, "gtk__menu_bar_E");
   E703 : Short_Integer; pragma Import (Ada, E703, "gtk__scrollbar_E");
   E701 : Short_Integer; pragma Import (Ada, E701, "gtk__scrolled_window_E");
   E707 : Short_Integer; pragma Import (Ada, E707, "gtk__separator_E");
   E709 : Short_Integer; pragma Import (Ada, E709, "gtk__separator_menu_item_E");
   E395 : Short_Integer; pragma Import (Ada, E395, "gtk__spin_button_E");
   E393 : Short_Integer; pragma Import (Ada, E393, "gtk__toggle_button_E");
   E391 : Short_Integer; pragma Import (Ada, E391, "gtk__check_button_E");
   E389 : Short_Integer; pragma Import (Ada, E389, "gtk__radio_button_E");
   E399 : Short_Integer; pragma Import (Ada, E399, "gtk__tooltip_E");
   E439 : Short_Integer; pragma Import (Ada, E439, "gtk__tree_drag_dest_E");
   E441 : Short_Integer; pragma Import (Ada, E441, "gtk__tree_drag_source_E");
   E401 : Short_Integer; pragma Import (Ada, E401, "gtk__tree_selection_E");
   E421 : Short_Integer; pragma Import (Ada, E421, "gtk__tree_sortable_E");
   E437 : Short_Integer; pragma Import (Ada, E437, "gtk__tree_store_E");
   E383 : Short_Integer; pragma Import (Ada, E383, "gtk__tree_view_column_E");
   E397 : Short_Integer; pragma Import (Ada, E397, "gtk__tree_view_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "callbacks_pt_editor_E");
   E447 : Short_Integer; pragma Import (Ada, E447, "gtk__combo_box_E");
   E445 : Short_Integer; pragma Import (Ada, E445, "gtk__combo_box_text_E");
   E402 : Short_Integer; pragma Import (Ada, E402, "gtkada__handlers_E");
   E677 : Short_Integer; pragma Import (Ada, E677, "priority_queues_E");
   E404 : Short_Integer; pragma Import (Ada, E404, "pt_editor_intl_E");
   E449 : Short_Integer; pragma Import (Ada, E449, "var_strings_E");
   E534 : Short_Integer; pragma Import (Ada, E534, "mast_E");
   E638 : Short_Integer; pragma Import (Ada, E638, "mast__tool_exceptions_E");
   E670 : Short_Integer; pragma Import (Ada, E670, "mast__annealing_parameters_E");
   E681 : Short_Integer; pragma Import (Ada, E681, "mast__hospa_parameters_E");
   E559 : Short_Integer; pragma Import (Ada, E559, "named_lists_E");
   E557 : Short_Integer; pragma Import (Ada, E557, "symbol_table_E");
   E555 : Short_Integer; pragma Import (Ada, E555, "mast_parser_tokens_E");
   E543 : Short_Integer; pragma Import (Ada, E543, "mast__io_E");
   E619 : Short_Integer; pragma Import (Ada, E619, "mast__scheduling_parameters_E");
   E603 : Short_Integer; pragma Import (Ada, E603, "mast__scheduling_policies_E");
   E627 : Short_Integer; pragma Import (Ada, E627, "mast__synchronization_parameters_E");
   E575 : Short_Integer; pragma Import (Ada, E575, "mast__events_E");
   E579 : Short_Integer; pragma Import (Ada, E579, "mast__graphs_E");
   E587 : Short_Integer; pragma Import (Ada, E587, "mast__results_E");
   E585 : Short_Integer; pragma Import (Ada, E585, "mast__processing_resources_E");
   E601 : Short_Integer; pragma Import (Ada, E601, "mast__schedulers_E");
   E623 : Short_Integer; pragma Import (Ada, E623, "mast__scheduling_servers_E");
   E629 : Short_Integer; pragma Import (Ada, E629, "mast__schedulers__adjustment_E");
   E625 : Short_Integer; pragma Import (Ada, E625, "mast__schedulers__secondary_E");
   E621 : Short_Integer; pragma Import (Ada, E621, "mast__shared_resources_E");
   E617 : Short_Integer; pragma Import (Ada, E617, "mast__operations_E");
   E615 : Short_Integer; pragma Import (Ada, E615, "mast__drivers_E");
   E583 : Short_Integer; pragma Import (Ada, E583, "mast__graphs__event_handlers_E");
   E599 : Short_Integer; pragma Import (Ada, E599, "mast__processing_resources__network_E");
   E613 : Short_Integer; pragma Import (Ada, E613, "mast__timers_E");
   E609 : Short_Integer; pragma Import (Ada, E609, "mast__processing_resources__processor_E");
   E593 : Short_Integer; pragma Import (Ada, E593, "mast__timing_requirements_E");
   E591 : Short_Integer; pragma Import (Ada, E591, "mast__graphs__links_E");
   E577 : Short_Integer; pragma Import (Ada, E577, "mast__transactions_E");
   E597 : Short_Integer; pragma Import (Ada, E597, "mast__systems_E");
   E607 : Short_Integer; pragma Import (Ada, E607, "mast__schedulers__primary_E");
   E595 : Short_Integer; pragma Import (Ada, E595, "mast__xmi_E");
   E640 : Short_Integer; pragma Import (Ada, E640, "mast__transaction_operations_E");
   E647 : Short_Integer; pragma Import (Ada, E647, "mast__consistency_checks_E");
   E653 : Short_Integer; pragma Import (Ada, E653, "mast__max_numbers_E");
   E642 : Short_Integer; pragma Import (Ada, E642, "trimmed_image_E");
   E651 : Short_Integer; pragma Import (Ada, E651, "mast__linear_analysis_tools_E");
   E687 : Short_Integer; pragma Import (Ada, E687, "mast__miscelaneous_tools_E");
   E631 : Short_Integer; pragma Import (Ada, E631, "mast__restrictions_E");
   E644 : Short_Integer; pragma Import (Ada, E644, "mast__tools_E");
   E657 : Short_Integer; pragma Import (Ada, E657, "mast__linear_priority_assignment_tools_E");
   E679 : Short_Integer; pragma Import (Ada, E679, "mast__linear_scheduling_parameters_assignment_tools_E");
   E636 : Short_Integer; pragma Import (Ada, E636, "mast__linear_translation_E");
   E685 : Short_Integer; pragma Import (Ada, E685, "mast__linear_task_analysis_tools_E");
   E691 : Short_Integer; pragma Import (Ada, E691, "mast__monoprocessor_tools_E");
   E675 : Short_Integer; pragma Import (Ada, E675, "mast__tools__schedulability_index_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "aboutdialog1_pkg_E");
   E502 : Short_Integer; pragma Import (Ada, E502, "file_operations_E");
   E520 : Short_Integer; pragma Import (Ada, E520, "filechooserdialog1_pkg_E");
   E522 : Short_Integer; pragma Import (Ada, E522, "filechooserdialog1_pkg__callbacks_E");
   E532 : Short_Integer; pragma Import (Ada, E532, "global_options_E");
   E573 : Short_Integer; pragma Import (Ada, E573, "model_operations_E");
   E423 : Short_Integer; pragma Import (Ada, E423, "mutex_table_E");
   E408 : Short_Integer; pragma Import (Ada, E408, "pt_editor_pkg_E");
   E488 : Short_Integer; pragma Import (Ada, E488, "dialog1_pkg_E");
   E490 : Short_Integer; pragma Import (Ada, E490, "dialog1_pkg__callbacks_E");
   E494 : Short_Integer; pragma Import (Ada, E494, "dialog_3_pkg_E");
   E496 : Short_Integer; pragma Import (Ada, E496, "dialog_3_pkg__callbacks_E");
   E498 : Short_Integer; pragma Import (Ada, E498, "dialog_yes_no_pkg_E");
   E500 : Short_Integer; pragma Import (Ada, E500, "dialog_yes_no_pkg__callbacks_E");
   E459 : Short_Integer; pragma Import (Ada, E459, "pt_editor_pkg__callbacks_E");
   E541 : Short_Integer; pragma Import (Ada, E541, "results_table_E");
   E567 : Short_Integer; pragma Import (Ada, E567, "task_table_E");
   E693 : Short_Integer; pragma Import (Ada, E693, "usage_dialog_pkg_E");
   E695 : Short_Integer; pragma Import (Ada, E695, "usage_dialog_pkg__callbacks_E");
   E569 : Short_Integer; pragma Import (Ada, E569, "usage_table_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E408 := E408 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "model_operations__finalize_body");
      begin
         E573 := E573 - 1;
         F1;
      end;
      E693 := E693 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "usage_dialog_pkg__finalize_spec");
      begin
         F2;
      end;
      E520 := E520 - 1;
      E498 := E498 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "dialog_yes_no_pkg__finalize_spec");
      begin
         F3;
      end;
      E494 := E494 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "dialog_3_pkg__finalize_spec");
      begin
         F4;
      end;
      E488 := E488 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "dialog1_pkg__finalize_spec");
      begin
         F5;
      end;
      E118 := E118 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "pt_editor_pkg__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "filechooserdialog1_pkg__finalize_spec");
      begin
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "aboutdialog1_pkg__finalize_spec");
      begin
         F8;
      end;
      E577 := E577 - 1;
      E593 := E593 - 1;
      E613 := E613 - 1;
      E597 := E597 - 1;
      E621 := E621 - 1;
      E623 := E623 - 1;
      E625 := E625 - 1;
      E607 := E607 - 1;
      E587 := E587 - 1;
      E609 := E609 - 1;
      E599 := E599 - 1;
      E617 := E617 - 1;
      E583 := E583 - 1;
      E575 := E575 - 1;
      E615 := E615 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "mast__schedulers__primary__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "mast__systems__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "mast__transactions__finalize_spec");
      begin
         F11;
      end;
      E591 := E591 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "mast__graphs__links__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "mast__timing_requirements__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "mast__processing_resources__processor__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "mast__timers__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "mast__processing_resources__network__finalize_spec");
      begin
         F16;
      end;
      declare
         procedure F17;
         pragma Import (Ada, F17, "mast__graphs__event_handlers__finalize_spec");
      begin
         F17;
      end;
      declare
         procedure F18;
         pragma Import (Ada, F18, "mast__drivers__finalize_spec");
      begin
         F18;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "mast__operations__finalize_spec");
      begin
         F19;
      end;
      declare
         procedure F20;
         pragma Import (Ada, F20, "mast__shared_resources__finalize_spec");
      begin
         F20;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "mast__schedulers__secondary__finalize_spec");
      begin
         F21;
      end;
      declare
         procedure F22;
         pragma Import (Ada, F22, "mast__scheduling_servers__finalize_spec");
      begin
         F22;
      end;
      E601 := E601 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "mast__schedulers__finalize_spec");
      begin
         F23;
      end;
      E585 := E585 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "mast__processing_resources__finalize_spec");
      begin
         F24;
      end;
      declare
         procedure F25;
         pragma Import (Ada, F25, "mast__results__finalize_spec");
      begin
         F25;
      end;
      E579 := E579 - 1;
      declare
         procedure F26;
         pragma Import (Ada, F26, "mast__graphs__finalize_spec");
      begin
         F26;
      end;
      declare
         procedure F27;
         pragma Import (Ada, F27, "mast__events__finalize_spec");
      begin
         F27;
      end;
      E627 := E627 - 1;
      declare
         procedure F28;
         pragma Import (Ada, F28, "mast__synchronization_parameters__finalize_spec");
      begin
         F28;
      end;
      E603 := E603 - 1;
      declare
         procedure F29;
         pragma Import (Ada, F29, "mast__scheduling_policies__finalize_spec");
      begin
         F29;
      end;
      E619 := E619 - 1;
      declare
         procedure F30;
         pragma Import (Ada, F30, "mast__scheduling_parameters__finalize_spec");
      begin
         F30;
      end;
      declare
         procedure F31;
         pragma Import (Ada, F31, "gtkada__handlers__finalize_spec");
      begin
         E402 := E402 - 1;
         F31;
      end;
      E445 := E445 - 1;
      declare
         procedure F32;
         pragma Import (Ada, F32, "gtk__combo_box_text__finalize_spec");
      begin
         F32;
      end;
      E447 := E447 - 1;
      declare
         procedure F33;
         pragma Import (Ada, F33, "gtk__combo_box__finalize_spec");
      begin
         F33;
      end;
      declare
         procedure F34;
         pragma Import (Ada, F34, "callbacks_pt_editor__finalize_spec");
      begin
         E119 := E119 - 1;
         F34;
      end;
      E397 := E397 - 1;
      declare
         procedure F35;
         pragma Import (Ada, F35, "gtk__tree_view__finalize_spec");
      begin
         F35;
      end;
      E383 := E383 - 1;
      declare
         procedure F36;
         pragma Import (Ada, F36, "gtk__tree_view_column__finalize_spec");
      begin
         F36;
      end;
      E437 := E437 - 1;
      declare
         procedure F37;
         pragma Import (Ada, F37, "gtk__tree_store__finalize_spec");
      begin
         F37;
      end;
      E401 := E401 - 1;
      declare
         procedure F38;
         pragma Import (Ada, F38, "gtk__tree_selection__finalize_spec");
      begin
         F38;
      end;
      E399 := E399 - 1;
      declare
         procedure F39;
         pragma Import (Ada, F39, "gtk__tooltip__finalize_spec");
      begin
         F39;
      end;
      E389 := E389 - 1;
      declare
         procedure F40;
         pragma Import (Ada, F40, "gtk__radio_button__finalize_spec");
      begin
         F40;
      end;
      E391 := E391 - 1;
      declare
         procedure F41;
         pragma Import (Ada, F41, "gtk__check_button__finalize_spec");
      begin
         F41;
      end;
      E393 := E393 - 1;
      declare
         procedure F42;
         pragma Import (Ada, F42, "gtk__toggle_button__finalize_spec");
      begin
         F42;
      end;
      E395 := E395 - 1;
      declare
         procedure F43;
         pragma Import (Ada, F43, "gtk__spin_button__finalize_spec");
      begin
         F43;
      end;
      E709 := E709 - 1;
      declare
         procedure F44;
         pragma Import (Ada, F44, "gtk__separator_menu_item__finalize_spec");
      begin
         F44;
      end;
      E707 := E707 - 1;
      declare
         procedure F45;
         pragma Import (Ada, F45, "gtk__separator__finalize_spec");
      begin
         F45;
      end;
      E701 := E701 - 1;
      declare
         procedure F46;
         pragma Import (Ada, F46, "gtk__scrolled_window__finalize_spec");
      begin
         F46;
      end;
      E703 := E703 - 1;
      declare
         procedure F47;
         pragma Import (Ada, F47, "gtk__scrollbar__finalize_spec");
      begin
         F47;
      end;
      E699 := E699 - 1;
      declare
         procedure F48;
         pragma Import (Ada, F48, "gtk__menu_bar__finalize_spec");
      begin
         F48;
      end;
      E429 := E429 - 1;
      declare
         procedure F49;
         pragma Import (Ada, F49, "gtk__label__finalize_spec");
      begin
         F49;
      end;
      E431 := E431 - 1;
      declare
         procedure F50;
         pragma Import (Ada, F50, "gtk__menu__finalize_spec");
      begin
         F50;
      end;
      E435 := E435 - 1;
      declare
         procedure F51;
         pragma Import (Ada, F51, "gtk__menu_shell__finalize_spec");
      begin
         F51;
      end;
      E385 := E385 - 1;
      declare
         procedure F52;
         pragma Import (Ada, F52, "gtk__image_menu_item__finalize_spec");
      begin
         F52;
      end;
      E387 := E387 - 1;
      declare
         procedure F53;
         pragma Import (Ada, F53, "gtk__menu_item__finalize_spec");
      begin
         F53;
      end;
      E705 := E705 - 1;
      declare
         procedure F54;
         pragma Import (Ada, F54, "gtk__grange__finalize_spec");
      begin
         F54;
      end;
      E419 := E419 - 1;
      declare
         procedure F55;
         pragma Import (Ada, F55, "gtk__frame__finalize_spec");
      begin
         F55;
      end;
      E528 := E528 - 1;
      declare
         procedure F56;
         pragma Import (Ada, F56, "gtk__file_chooser_dialog__finalize_spec");
      begin
         F56;
      end;
      E417 := E417 - 1;
      declare
         procedure F57;
         pragma Import (Ada, F57, "gtk__cell_renderer_text__finalize_spec");
      begin
         F57;
      end;
      E178 := E178 - 1;
      declare
         procedure F58;
         pragma Import (Ada, F58, "gtk__button__finalize_spec");
      begin
         F58;
      end;
      E492 := E492 - 1;
      declare
         procedure F59;
         pragma Import (Ada, F59, "gtk__alignment__finalize_spec");
      begin
         F59;
      end;
      E369 := E369 - 1;
      declare
         procedure F60;
         pragma Import (Ada, F60, "gtk__action__finalize_spec");
      begin
         F60;
      end;
      E711 := E711 - 1;
      declare
         procedure F61;
         pragma Import (Ada, F61, "gtk__about_dialog__finalize_spec");
      begin
         F61;
      end;
      E433 := E433 - 1;
      declare
         procedure F62;
         pragma Import (Ada, F62, "glib__menu_model__finalize_spec");
      begin
         F62;
      end;
      E287 := E287 - 1;
      E220 := E220 - 1;
      E317 := E317 - 1;
      E355 := E355 - 1;
      E367 := E367 - 1;
      E359 := E359 - 1;
      E325 := E325 - 1;
      E244 := E244 - 1;
      E347 := E347 - 1;
      E333 := E333 - 1;
      E331 := E331 - 1;
      E297 := E297 - 1;
      E307 := E307 - 1;
      E305 := E305 - 1;
      E212 := E212 - 1;
      E281 := E281 - 1;
      E361 := E361 - 1;
      E315 := E315 - 1;
      E309 := E309 - 1;
      E283 := E283 - 1;
      E238 := E238 - 1;
      E204 := E204 - 1;
      E224 := E224 - 1;
      E187 := E187 - 1;
      E193 := E193 - 1;
      E191 := E191 - 1;
      declare
         procedure F63;
         pragma Import (Ada, F63, "gtk__print_operation__finalize_spec");
      begin
         F63;
      end;
      declare
         procedure F64;
         pragma Import (Ada, F64, "gtk__dialog__finalize_spec");
      begin
         F64;
      end;
      declare
         procedure F65;
         pragma Import (Ada, F65, "gtk__window__finalize_spec");
      begin
         F65;
      end;
      declare
         procedure F66;
         pragma Import (Ada, F66, "gtk__text_view__finalize_spec");
      begin
         F66;
      end;
      declare
         procedure F67;
         pragma Import (Ada, F67, "gtk__text_buffer__finalize_spec");
      begin
         F67;
      end;
      E363 := E363 - 1;
      declare
         procedure F68;
         pragma Import (Ada, F68, "gtk__text_child_anchor__finalize_spec");
      begin
         F68;
      end;
      declare
         procedure F69;
         pragma Import (Ada, F69, "gtk__gentry__finalize_spec");
      begin
         F69;
      end;
      E319 := E319 - 1;
      declare
         procedure F70;
         pragma Import (Ada, F70, "gtk__image__finalize_spec");
      begin
         F70;
      end;
      E321 := E321 - 1;
      declare
         procedure F71;
         pragma Import (Ada, F71, "gtk__icon_set__finalize_spec");
      begin
         F71;
      end;
      declare
         procedure F72;
         pragma Import (Ada, F72, "gtk__style_context__finalize_spec");
      begin
         F72;
      end;
      E214 := E214 - 1;
      declare
         procedure F73;
         pragma Import (Ada, F73, "gtk__settings__finalize_spec");
      begin
         F73;
      end;
      declare
         procedure F74;
         pragma Import (Ada, F74, "gtk__status_bar__finalize_spec");
      begin
         F74;
      end;
      declare
         procedure F75;
         pragma Import (Ada, F75, "gtk__notebook__finalize_spec");
      begin
         F75;
      end;
      E329 := E329 - 1;
      declare
         procedure F76;
         pragma Import (Ada, F76, "gtk__misc__finalize_spec");
      begin
         F76;
      end;
      declare
         procedure F77;
         pragma Import (Ada, F77, "gtk__entry_completion__finalize_spec");
      begin
         F77;
      end;
      E277 := E277 - 1;
      declare
         procedure F78;
         pragma Import (Ada, F78, "gtk__box__finalize_spec");
      begin
         F78;
      end;
      E295 := E295 - 1;
      declare
         procedure F79;
         pragma Import (Ada, F79, "gtk__bin__finalize_spec");
      begin
         F79;
      end;
      declare
         procedure F80;
         pragma Import (Ada, F80, "gtk__container__finalize_spec");
      begin
         F80;
      end;
      declare
         procedure F81;
         pragma Import (Ada, F81, "gtk__cell_area__finalize_spec");
      begin
         F81;
      end;
      declare
         procedure F82;
         pragma Import (Ada, F82, "gtk__cell_renderer__finalize_spec");
      begin
         F82;
      end;
      declare
         procedure F83;
         pragma Import (Ada, F83, "gtk__widget__finalize_spec");
      begin
         F83;
      end;
      declare
         procedure F84;
         pragma Import (Ada, F84, "gtk__tree_model__finalize_spec");
      begin
         F84;
      end;
      declare
         procedure F85;
         pragma Import (Ada, F85, "gtk__text_tag_table__finalize_spec");
      begin
         F85;
      end;
      declare
         procedure F86;
         pragma Import (Ada, F86, "gtk__style__finalize_spec");
      begin
         F86;
      end;
      declare
         procedure F87;
         pragma Import (Ada, F87, "gtk__clipboard__finalize_spec");
      begin
         F87;
      end;
      E242 := E242 - 1;
      declare
         procedure F88;
         pragma Import (Ada, F88, "gtk__selection_data__finalize_spec");
      begin
         F88;
      end;
      E323 := E323 - 1;
      declare
         procedure F89;
         pragma Import (Ada, F89, "gtk__icon_source__finalize_spec");
      begin
         F89;
      end;
      declare
         procedure F90;
         pragma Import (Ada, F90, "gtk__entry_buffer__finalize_spec");
      begin
         F90;
      end;
      declare
         procedure F91;
         pragma Import (Ada, F91, "gtk__adjustment__finalize_spec");
      begin
         F91;
      end;
      declare
         procedure F92;
         pragma Import (Ada, F92, "gtk__accel_group__finalize_spec");
      begin
         F92;
      end;
      declare
         procedure F93;
         pragma Import (Ada, F93, "gdk__drag_contexts__finalize_spec");
      begin
         F93;
      end;
      declare
         procedure F94;
         pragma Import (Ada, F94, "gdk__device__finalize_spec");
      begin
         F94;
      end;
      E206 := E206 - 1;
      declare
         procedure F95;
         pragma Import (Ada, F95, "gdk__screen__finalize_spec");
      begin
         F95;
      end;
      E228 := E228 - 1;
      declare
         procedure F96;
         pragma Import (Ada, F96, "gdk__pixbuf__finalize_spec");
      begin
         F96;
      end;
      E293 := E293 - 1;
      declare
         procedure F97;
         pragma Import (Ada, F97, "gdk__glcontext__finalize_spec");
      begin
         F97;
      end;
      declare
         procedure F98;
         pragma Import (Ada, F98, "gdk__display__finalize_spec");
      begin
         F98;
      end;
      declare
         procedure F99;
         pragma Import (Ada, F99, "gdk__monitor__finalize_spec");
      begin
         F99;
      end;
      declare
         procedure F100;
         pragma Import (Ada, F100, "gdk__frame_clock__finalize_spec");
      begin
         F100;
      end;
      E341 := E341 - 1;
      declare
         procedure F101;
         pragma Import (Ada, F101, "gtk__print_context__finalize_spec");
      begin
         F101;
      end;
      E271 := E271 - 1;
      declare
         procedure F102;
         pragma Import (Ada, F102, "pango__layout__finalize_spec");
      begin
         F102;
      end;
      E275 := E275 - 1;
      declare
         procedure F103;
         pragma Import (Ada, F103, "pango__tabs__finalize_spec");
      begin
         F103;
      end;
      E269 := E269 - 1;
      declare
         procedure F104;
         pragma Import (Ada, F104, "pango__font_map__finalize_spec");
      begin
         F104;
      end;
      E251 := E251 - 1;
      declare
         procedure F105;
         pragma Import (Ada, F105, "pango__context__finalize_spec");
      begin
         F105;
      end;
      E265 := E265 - 1;
      declare
         procedure F106;
         pragma Import (Ada, F106, "pango__fontset__finalize_spec");
      begin
         F106;
      end;
      E261 := E261 - 1;
      declare
         procedure F107;
         pragma Import (Ada, F107, "pango__font_family__finalize_spec");
      begin
         F107;
      end;
      E263 := E263 - 1;
      declare
         procedure F108;
         pragma Import (Ada, F108, "pango__font_face__finalize_spec");
      begin
         F108;
      end;
      E353 := E353 - 1;
      declare
         procedure F109;
         pragma Import (Ada, F109, "gtk__text_tag__finalize_spec");
      begin
         F109;
      end;
      E255 := E255 - 1;
      declare
         procedure F110;
         pragma Import (Ada, F110, "pango__font__finalize_spec");
      begin
         F110;
      end;
      E259 := E259 - 1;
      declare
         procedure F111;
         pragma Import (Ada, F111, "pango__language__finalize_spec");
      begin
         F111;
      end;
      E257 := E257 - 1;
      declare
         procedure F112;
         pragma Import (Ada, F112, "pango__font_metrics__finalize_spec");
      begin
         F112;
      end;
      E273 := E273 - 1;
      declare
         procedure F113;
         pragma Import (Ada, F113, "pango__attributes__finalize_spec");
      begin
         F113;
      end;
      E365 := E365 - 1;
      declare
         procedure F114;
         pragma Import (Ada, F114, "gtk__text_mark__finalize_spec");
      begin
         F114;
      end;
      E246 := E246 - 1;
      declare
         procedure F115;
         pragma Import (Ada, F115, "gtk__target_list__finalize_spec");
      begin
         F115;
      end;
      E345 := E345 - 1;
      declare
         procedure F116;
         pragma Import (Ada, F116, "gtk__print_settings__finalize_spec");
      begin
         F116;
      end;
      E335 := E335 - 1;
      declare
         procedure F117;
         pragma Import (Ada, F117, "gtk__page_setup__finalize_spec");
      begin
         F117;
      end;
      E339 := E339 - 1;
      declare
         procedure F118;
         pragma Import (Ada, F118, "gtk__paper_size__finalize_spec");
      begin
         F118;
      end;
      E526 := E526 - 1;
      declare
         procedure F119;
         pragma Import (Ada, F119, "gtk__file_filter__finalize_spec");
      begin
         F119;
      end;
      E327 := E327 - 1;
      declare
         procedure F120;
         pragma Import (Ada, F120, "gtk__css_section__finalize_spec");
      begin
         F120;
      end;
      E311 := E311 - 1;
      declare
         procedure F121;
         pragma Import (Ada, F121, "gtk__cell_area_context__finalize_spec");
      begin
         F121;
      end;
      E240 := E240 - 1;
      declare
         procedure F122;
         pragma Import (Ada, F122, "gtk__builder__finalize_spec");
      begin
         F122;
      end;
      E234 := E234 - 1;
      declare
         procedure F123;
         pragma Import (Ada, F123, "glib__variant__finalize_spec");
      begin
         F123;
      end;
      E291 := E291 - 1;
      declare
         procedure F124;
         pragma Import (Ada, F124, "gdk__drawing_context__finalize_spec");
      begin
         F124;
      end;
      E197 := E197 - 1;
      declare
         procedure F125;
         pragma Import (Ada, F125, "gdk__device_tool__finalize_spec");
      begin
         F125;
      end;
      E153 := E153 - 1;
      declare
         procedure F126;
         pragma Import (Ada, F126, "glib__object__finalize_spec");
      begin
         F126;
      end;
      E226 := E226 - 1;
      declare
         procedure F127;
         pragma Import (Ada, F127, "gdk__frame_timings__finalize_spec");
      begin
         F127;
      end;
      E121 := E121 - 1;
      declare
         procedure F128;
         pragma Import (Ada, F128, "glib__finalize_spec");
      begin
         F128;
      end;
      declare
         procedure F129;
         pragma Import (Ada, F129, "ada__directories__finalize_body");
      begin
         E465 := E465 - 1;
         F129;
      end;
      declare
         procedure F130;
         pragma Import (Ada, F130, "ada__directories__finalize_spec");
      begin
         F130;
      end;
      E475 := E475 - 1;
      declare
         procedure F131;
         pragma Import (Ada, F131, "system__regexp__finalize_spec");
      begin
         F131;
      end;
      E141 := E141 - 1;
      declare
         procedure F132;
         pragma Import (Ada, F132, "system__pool_global__finalize_spec");
      begin
         F132;
      end;
      E410 := E410 - 1;
      declare
         procedure F133;
         pragma Import (Ada, F133, "ada__text_io__finalize_spec");
      begin
         F133;
      end;
      E451 := E451 - 1;
      declare
         procedure F134;
         pragma Import (Ada, F134, "ada__strings__unbounded__finalize_spec");
      begin
         F134;
      end;
      E157 := E157 - 1;
      declare
         procedure F135;
         pragma Import (Ada, F135, "system__storage_pools__subpools__finalize_spec");
      begin
         F135;
      end;
      E134 := E134 - 1;
      declare
         procedure F136;
         pragma Import (Ada, F136, "system__finalization_masters__finalize_spec");
      begin
         F136;
      end;
      declare
         procedure F137;
         pragma Import (Ada, F137, "system__file_io__finalize_body");
      begin
         E414 := E414 - 1;
         F137;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E010 := E010 + 1;
      Ada.Containers'Elab_Spec;
      E035 := E035 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E064 := E064 + 1;
      Ada.Numerics'Elab_Spec;
      E025 := E025 + 1;
      Ada.Strings'Elab_Spec;
      E007 := E007 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E053 := E053 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E056 := E056 + 1;
      Interfaces.C'Elab_Spec;
      E040 := E040 + 1;
      System.Exceptions'Elab_Spec;
      E019 := E019 + 1;
      System.Object_Reader'Elab_Spec;
      E080 := E080 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E047 := E047 + 1;
      System.Os_Lib'Elab_Body;
      E069 := E069 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E099 := E099 + 1;
      E012 := E012 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E034 := E034 + 1;
      E016 := E016 + 1;
      Ada.Assertions'Elab_Spec;
      E379 := E379 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E103 := E103 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E111 := E111 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E005 := E005 + 1;
      Gnat'Elab_Spec;
      E162 := E162 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E126 := E126 + 1;
      Ada.Streams'Elab_Spec;
      E128 := E128 + 1;
      System.File_Control_Block'Elab_Spec;
      E415 := E415 + 1;
      System.Finalization_Root'Elab_Spec;
      E137 := E137 + 1;
      Ada.Finalization'Elab_Spec;
      E135 := E135 + 1;
      System.File_Io'Elab_Body;
      E414 := E414 + 1;
      System.Storage_Pools'Elab_Spec;
      E139 := E139 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E134 := E134 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E157 := E157 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E451 := E451 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E461 := E461 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E410 := E410 + 1;
      System.Pool_Global'Elab_Spec;
      E141 := E141 + 1;
      System.Random_Seed'Elab_Body;
      E668 := E668 + 1;
      System.Regexp'Elab_Spec;
      E475 := E475 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E465 := E465 + 1;
      Glib'Elab_Spec;
      Gtkada.Types'Elab_Spec;
      E124 := E124 + 1;
      E121 := E121 + 1;
      E554 := E554 + 1;
      E477 := E477 + 1;
      E479 := E479 + 1;
      Gdk.Frame_Timings'Elab_Spec;
      Gdk.Frame_Timings'Elab_Body;
      E226 := E226 + 1;
      E173 := E173 + 1;
      Gdk.Visual'Elab_Body;
      E208 := E208 + 1;
      E175 := E175 + 1;
      E167 := E167 + 1;
      Glib.Object'Elab_Spec;
      E155 := E155 + 1;
      Glib.Values'Elab_Body;
      E171 := E171 + 1;
      E161 := E161 + 1;
      Glib.Object'Elab_Body;
      E153 := E153 + 1;
      E169 := E169 + 1;
      E182 := E182 + 1;
      E184 := E184 + 1;
      E201 := E201 + 1;
      Glib.Generic_Properties'Elab_Spec;
      Glib.Generic_Properties'Elab_Body;
      E189 := E189 + 1;
      Gdk.Color'Elab_Spec;
      E222 := E222 + 1;
      E210 := E210 + 1;
      E337 := E337 + 1;
      E199 := E199 + 1;
      Gdk.Device_Tool'Elab_Spec;
      Gdk.Device_Tool'Elab_Body;
      E197 := E197 + 1;
      Gdk.Drawing_Context'Elab_Spec;
      Gdk.Drawing_Context'Elab_Body;
      E291 := E291 + 1;
      E195 := E195 + 1;
      E236 := E236 + 1;
      Glib.Variant'Elab_Spec;
      Glib.Variant'Elab_Body;
      E234 := E234 + 1;
      E299 := E299 + 1;
      Gtk.Actionable'Elab_Spec;
      E371 := E371 + 1;
      Gtk.Builder'Elab_Spec;
      Gtk.Builder'Elab_Body;
      E240 := E240 + 1;
      E279 := E279 + 1;
      Gtk.Cell_Area_Context'Elab_Spec;
      Gtk.Cell_Area_Context'Elab_Body;
      E311 := E311 + 1;
      Gtk.Css_Section'Elab_Spec;
      Gtk.Css_Section'Elab_Body;
      E327 := E327 + 1;
      E216 := E216 + 1;
      Gtk.File_Filter'Elab_Spec;
      Gtk.File_Filter'Elab_Body;
      E526 := E526 + 1;
      Gtk.Orientable'Elab_Spec;
      E285 := E285 + 1;
      Gtk.Paper_Size'Elab_Spec;
      Gtk.Paper_Size'Elab_Body;
      E339 := E339 + 1;
      Gtk.Page_Setup'Elab_Spec;
      Gtk.Page_Setup'Elab_Body;
      E335 := E335 + 1;
      Gtk.Print_Settings'Elab_Spec;
      Gtk.Print_Settings'Elab_Body;
      E345 := E345 + 1;
      E530 := E530 + 1;
      E248 := E248 + 1;
      Gtk.Target_List'Elab_Spec;
      Gtk.Target_List'Elab_Body;
      E246 := E246 + 1;
      Gtk.Text_Mark'Elab_Spec;
      Gtk.Text_Mark'Elab_Body;
      E365 := E365 + 1;
      List_Exceptions'Elab_Spec;
      E560 := E560 + 1;
      E649 := E649 + 1;
      E683 := E683 + 1;
      E689 := E689 + 1;
      E589 := E589 + 1;
      E581 := E581 + 1;
      E253 := E253 + 1;
      Pango.Attributes'Elab_Spec;
      Pango.Attributes'Elab_Body;
      E273 := E273 + 1;
      Pango.Font_Metrics'Elab_Spec;
      Pango.Font_Metrics'Elab_Body;
      E257 := E257 + 1;
      Pango.Language'Elab_Spec;
      Pango.Language'Elab_Body;
      E259 := E259 + 1;
      Pango.Font'Elab_Spec;
      Pango.Font'Elab_Body;
      E255 := E255 + 1;
      E351 := E351 + 1;
      Gtk.Text_Tag'Elab_Spec;
      Gtk.Text_Tag'Elab_Body;
      E353 := E353 + 1;
      Pango.Font_Face'Elab_Spec;
      Pango.Font_Face'Elab_Body;
      E263 := E263 + 1;
      Pango.Font_Family'Elab_Spec;
      Pango.Font_Family'Elab_Body;
      E261 := E261 + 1;
      Pango.Fontset'Elab_Spec;
      Pango.Fontset'Elab_Body;
      E265 := E265 + 1;
      E267 := E267 + 1;
      Pango.Context'Elab_Spec;
      Pango.Context'Elab_Body;
      E251 := E251 + 1;
      Pango.Font_Map'Elab_Spec;
      Pango.Font_Map'Elab_Body;
      E269 := E269 + 1;
      Pango.Tabs'Elab_Spec;
      Pango.Tabs'Elab_Body;
      E275 := E275 + 1;
      Pango.Layout'Elab_Spec;
      Pango.Layout'Elab_Body;
      E271 := E271 + 1;
      Gtk.Print_Context'Elab_Spec;
      Gtk.Print_Context'Elab_Body;
      E341 := E341 + 1;
      Gdk.Frame_Clock'Elab_Spec;
      Gdk.Monitor'Elab_Spec;
      Gdk.Display'Elab_Spec;
      Gdk.Glcontext'Elab_Spec;
      Gdk.Glcontext'Elab_Body;
      E293 := E293 + 1;
      Gdk.Pixbuf'Elab_Spec;
      E228 := E228 + 1;
      Gdk.Screen'Elab_Spec;
      Gdk.Screen'Elab_Body;
      E206 := E206 + 1;
      Gdk.Device'Elab_Spec;
      Gdk.Drag_Contexts'Elab_Spec;
      Gdk.Window'Elab_Spec;
      E289 := E289 + 1;
      Gtk.Accel_Group'Elab_Spec;
      Gtk.Adjustment'Elab_Spec;
      Gtk.Cell_Editable'Elab_Spec;
      Gtk.Entry_Buffer'Elab_Spec;
      Gtk.Icon_Source'Elab_Spec;
      Gtk.Icon_Source'Elab_Body;
      E323 := E323 + 1;
      Gtk.Selection_Data'Elab_Spec;
      Gtk.Selection_Data'Elab_Body;
      E242 := E242 + 1;
      Gtk.Clipboard'Elab_Spec;
      Gtk.Style'Elab_Spec;
      Gtk.Scrollable'Elab_Spec;
      E357 := E357 + 1;
      E349 := E349 + 1;
      Gtk.Text_Tag_Table'Elab_Spec;
      Gtk.Tree_Model'Elab_Spec;
      Gtk.Widget'Elab_Spec;
      Gtk.Cell_Renderer'Elab_Spec;
      E313 := E313 + 1;
      Gtk.Cell_Area'Elab_Spec;
      Gtk.Container'Elab_Spec;
      Gtk.Bin'Elab_Spec;
      Gtk.Bin'Elab_Body;
      E295 := E295 + 1;
      Gtk.Box'Elab_Spec;
      Gtk.Box'Elab_Body;
      E277 := E277 + 1;
      Gtk.Entry_Completion'Elab_Spec;
      Gtk.Misc'Elab_Spec;
      Gtk.Misc'Elab_Body;
      E329 := E329 + 1;
      Gtk.Notebook'Elab_Spec;
      Gtk.Status_Bar'Elab_Spec;
      E218 := E218 + 1;
      Gtk.Settings'Elab_Spec;
      Gtk.Settings'Elab_Body;
      E214 := E214 + 1;
      Gtk.Style_Context'Elab_Spec;
      Gtk.Icon_Set'Elab_Spec;
      Gtk.Icon_Set'Elab_Body;
      E321 := E321 + 1;
      Gtk.Image'Elab_Spec;
      Gtk.Image'Elab_Body;
      E319 := E319 + 1;
      Gtk.Gentry'Elab_Spec;
      Gtk.Text_Child_Anchor'Elab_Spec;
      Gtk.Text_Child_Anchor'Elab_Body;
      E363 := E363 + 1;
      Gtk.Text_Buffer'Elab_Spec;
      Gtk.Text_View'Elab_Spec;
      Gtk.Window'Elab_Spec;
      Gtk.Dialog'Elab_Spec;
      Gtk.Print_Operation'Elab_Spec;
      E180 := E180 + 1;
      Gdk.Device'Elab_Body;
      E191 := E191 + 1;
      Gdk.Display'Elab_Body;
      E193 := E193 + 1;
      Gdk.Drag_Contexts'Elab_Body;
      E187 := E187 + 1;
      Gdk.Frame_Clock'Elab_Body;
      E224 := E224 + 1;
      Gdk.Monitor'Elab_Body;
      E204 := E204 + 1;
      E232 := E232 + 1;
      Gtk.Accel_Group'Elab_Body;
      E238 := E238 + 1;
      Gtk.Adjustment'Elab_Body;
      E283 := E283 + 1;
      Gtk.Cell_Area'Elab_Body;
      E309 := E309 + 1;
      E301 := E301 + 1;
      Gtk.Cell_Renderer'Elab_Body;
      E315 := E315 + 1;
      Gtk.Clipboard'Elab_Body;
      E361 := E361 + 1;
      Gtk.Container'Elab_Body;
      E281 := E281 + 1;
      Gtk.Dialog'Elab_Body;
      E212 := E212 + 1;
      E303 := E303 + 1;
      Gtk.Entry_Buffer'Elab_Body;
      E305 := E305 + 1;
      Gtk.Entry_Completion'Elab_Body;
      E307 := E307 + 1;
      Gtk.Gentry'Elab_Body;
      E297 := E297 + 1;
      Gtk.Notebook'Elab_Body;
      E331 := E331 + 1;
      Gtk.Print_Operation'Elab_Body;
      E333 := E333 + 1;
      E343 := E343 + 1;
      Gtk.Status_Bar'Elab_Body;
      E347 := E347 + 1;
      Gtk.Style'Elab_Body;
      E244 := E244 + 1;
      Gtk.Style_Context'Elab_Body;
      E325 := E325 + 1;
      Gtk.Text_Buffer'Elab_Body;
      E359 := E359 + 1;
      Gtk.Text_Tag_Table'Elab_Body;
      E367 := E367 + 1;
      Gtk.Text_View'Elab_Body;
      E355 := E355 + 1;
      Gtk.Tree_Model'Elab_Body;
      E317 := E317 + 1;
      Gtk.Widget'Elab_Body;
      E220 := E220 + 1;
      Gtk.Window'Elab_Body;
      E287 := E287 + 1;
      Glib.Menu_Model'Elab_Spec;
      Glib.Menu_Model'Elab_Body;
      E433 := E433 + 1;
      Gtk.About_Dialog'Elab_Spec;
      Gtk.About_Dialog'Elab_Body;
      E711 := E711 + 1;
      Gtk.Action'Elab_Spec;
      Gtk.Action'Elab_Body;
      E369 := E369 + 1;
      Gtk.Activatable'Elab_Spec;
      E373 := E373 + 1;
      Gtk.Alignment'Elab_Spec;
      Gtk.Alignment'Elab_Body;
      E492 := E492 + 1;
      Gtk.Button'Elab_Spec;
      Gtk.Button'Elab_Body;
      E178 := E178 + 1;
      Gtk.Cell_Renderer_Text'Elab_Spec;
      Gtk.Cell_Renderer_Text'Elab_Body;
      E417 := E417 + 1;
      Gtk.File_Chooser'Elab_Spec;
      E524 := E524 + 1;
      Gtk.File_Chooser_Dialog'Elab_Spec;
      Gtk.File_Chooser_Dialog'Elab_Body;
      E528 := E528 + 1;
      Gtk.Frame'Elab_Spec;
      Gtk.Frame'Elab_Body;
      E419 := E419 + 1;
      Gtk.Grange'Elab_Spec;
      Gtk.Grange'Elab_Body;
      E705 := E705 + 1;
      E571 := E571 + 1;
      E381 := E381 + 1;
      Gtk.Menu_Item'Elab_Spec;
      Gtk.Menu_Item'Elab_Body;
      E387 := E387 + 1;
      Gtk.Image_Menu_Item'Elab_Spec;
      Gtk.Image_Menu_Item'Elab_Body;
      E385 := E385 + 1;
      Gtk.Menu_Shell'Elab_Spec;
      Gtk.Menu_Shell'Elab_Body;
      E435 := E435 + 1;
      Gtk.Menu'Elab_Spec;
      Gtk.Menu'Elab_Body;
      E431 := E431 + 1;
      Gtk.Label'Elab_Spec;
      Gtk.Label'Elab_Body;
      E429 := E429 + 1;
      Gtk.Menu_Bar'Elab_Spec;
      Gtk.Menu_Bar'Elab_Body;
      E699 := E699 + 1;
      Gtk.Scrollbar'Elab_Spec;
      Gtk.Scrollbar'Elab_Body;
      E703 := E703 + 1;
      Gtk.Scrolled_Window'Elab_Spec;
      Gtk.Scrolled_Window'Elab_Body;
      E701 := E701 + 1;
      Gtk.Separator'Elab_Spec;
      Gtk.Separator'Elab_Body;
      E707 := E707 + 1;
      Gtk.Separator_Menu_Item'Elab_Spec;
      Gtk.Separator_Menu_Item'Elab_Body;
      E709 := E709 + 1;
      Gtk.Spin_Button'Elab_Spec;
      Gtk.Spin_Button'Elab_Body;
      E395 := E395 + 1;
      Gtk.Toggle_Button'Elab_Spec;
      Gtk.Toggle_Button'Elab_Body;
      E393 := E393 + 1;
      Gtk.Check_Button'Elab_Spec;
      Gtk.Check_Button'Elab_Body;
      E391 := E391 + 1;
      Gtk.Radio_Button'Elab_Spec;
      Gtk.Radio_Button'Elab_Body;
      E389 := E389 + 1;
      Gtk.Tooltip'Elab_Spec;
      Gtk.Tooltip'Elab_Body;
      E399 := E399 + 1;
      E439 := E439 + 1;
      E441 := E441 + 1;
      Gtk.Tree_Selection'Elab_Spec;
      Gtk.Tree_Selection'Elab_Body;
      E401 := E401 + 1;
      E421 := E421 + 1;
      Gtk.Tree_Store'Elab_Spec;
      Gtk.Tree_Store'Elab_Body;
      E437 := E437 + 1;
      Gtk.Tree_View_Column'Elab_Spec;
      Gtk.Tree_View_Column'Elab_Body;
      E383 := E383 + 1;
      Gtk.Tree_View'Elab_Spec;
      Gtk.Tree_View'Elab_Body;
      E397 := E397 + 1;
      Callbacks_Pt_Editor'Elab_Spec;
      E119 := E119 + 1;
      Gtk.Combo_Box'Elab_Spec;
      Gtk.Combo_Box'Elab_Body;
      E447 := E447 + 1;
      Gtk.Combo_Box_Text'Elab_Spec;
      Gtk.Combo_Box_Text'Elab_Body;
      E445 := E445 + 1;
      Gtkada.Handlers'Elab_Spec;
      E402 := E402 + 1;
      E677 := E677 + 1;
      E404 := E404 + 1;
      Var_Strings'Elab_Spec;
      E449 := E449 + 1;
      Mast'Elab_Spec;
      Mast'Elab_Body;
      E534 := E534 + 1;
      Mast.Tool_Exceptions'Elab_Spec;
      Mast.Tool_Exceptions'Elab_Body;
      E638 := E638 + 1;
      Mast.Annealing_Parameters'Elab_Body;
      E670 := E670 + 1;
      Mast.Hospa_Parameters'Elab_Body;
      E681 := E681 + 1;
      E559 := E559 + 1;
      Symbol_Table'Elab_Spec;
      Symbol_Table'Elab_Body;
      E557 := E557 + 1;
      Mast_Parser_Tokens'Elab_Spec;
      E555 := E555 + 1;
      Mast.Io'Elab_Body;
      E543 := E543 + 1;
      Mast.Scheduling_Parameters'Elab_Spec;
      Mast.Scheduling_Parameters'Elab_Body;
      E619 := E619 + 1;
      Mast.Scheduling_Policies'Elab_Spec;
      Mast.Scheduling_Policies'Elab_Body;
      E603 := E603 + 1;
      Mast.Synchronization_Parameters'Elab_Spec;
      Mast.Synchronization_Parameters'Elab_Body;
      E627 := E627 + 1;
      Mast.Events'Elab_Spec;
      Mast.Graphs'Elab_Spec;
      Mast.Graphs'Elab_Body;
      E579 := E579 + 1;
      Mast.Results'Elab_Spec;
      Mast.Processing_Resources'Elab_Spec;
      Mast.Processing_Resources'Elab_Body;
      E585 := E585 + 1;
      Mast.Schedulers'Elab_Spec;
      Mast.Schedulers'Elab_Body;
      E601 := E601 + 1;
      Mast.Scheduling_Servers'Elab_Spec;
      Mast.Schedulers.Secondary'Elab_Spec;
      Mast.Shared_Resources'Elab_Spec;
      Mast.Operations'Elab_Spec;
      Mast.Drivers'Elab_Spec;
      Mast.Graphs.Event_Handlers'Elab_Spec;
      Mast.Processing_Resources.Network'Elab_Spec;
      Mast.Timers'Elab_Spec;
      Mast.Processing_Resources.Processor'Elab_Spec;
      Mast.Timing_Requirements'Elab_Spec;
      Mast.Graphs.Links'Elab_Spec;
      Mast.Graphs.Links'Elab_Body;
      E591 := E591 + 1;
      Mast.Transactions'Elab_Spec;
      Mast.Systems'Elab_Spec;
      Mast.Schedulers.Primary'Elab_Spec;
      E629 := E629 + 1;
      Mast.Xmi'Elab_Body;
      E595 := E595 + 1;
      Mast.Drivers'Elab_Body;
      E615 := E615 + 1;
      Mast.Events'Elab_Body;
      E575 := E575 + 1;
      Mast.Graphs.Event_Handlers'Elab_Body;
      E583 := E583 + 1;
      Mast.Operations'Elab_Body;
      E617 := E617 + 1;
      Mast.Processing_Resources.Network'Elab_Body;
      E599 := E599 + 1;
      Mast.Processing_Resources.Processor'Elab_Body;
      E609 := E609 + 1;
      Mast.Results'Elab_Body;
      E587 := E587 + 1;
      Mast.Schedulers.Primary'Elab_Body;
      E607 := E607 + 1;
      Mast.Schedulers.Secondary'Elab_Body;
      E625 := E625 + 1;
      Mast.Scheduling_Servers'Elab_Body;
      E623 := E623 + 1;
      Mast.Shared_Resources'Elab_Body;
      E621 := E621 + 1;
      Mast.Systems'Elab_Body;
      E597 := E597 + 1;
      Mast.Timers'Elab_Body;
      E613 := E613 + 1;
      Mast.Timing_Requirements'Elab_Body;
      E593 := E593 + 1;
      Mast.Transactions'Elab_Body;
      E577 := E577 + 1;
      Mast.Transaction_Operations'Elab_Spec;
      E640 := E640 + 1;
      Mast.Consistency_Checks'Elab_Body;
      E647 := E647 + 1;
      E653 := E653 + 1;
      E642 := E642 + 1;
      E636 := E636 + 1;
      E651 := E651 + 1;
      E685 := E685 + 1;
      Mast.Miscelaneous_Tools'Elab_Body;
      E687 := E687 + 1;
      E631 := E631 + 1;
      E644 := E644 + 1;
      Mast.Tools.Schedulability_Index'Elab_Spec;
      E675 := E675 + 1;
      E657 := E657 + 1;
      E679 := E679 + 1;
      E691 := E691 + 1;
      Aboutdialog1_Pkg'Elab_Spec;
      File_Operations'Elab_Spec;
      Filechooserdialog1_Pkg'Elab_Spec;
      E522 := E522 + 1;
      Mutex_Table'Elab_Spec;
      Pt_Editor_Pkg'Elab_Spec;
      E118 := E118 + 1;
      Dialog1_Pkg'Elab_Spec;
      E490 := E490 + 1;
      E488 := E488 + 1;
      Dialog_3_Pkg'Elab_Spec;
      E496 := E496 + 1;
      E494 := E494 + 1;
      Dialog_Yes_No_Pkg'Elab_Spec;
      E500 := E500 + 1;
      E498 := E498 + 1;
      E520 := E520 + 1;
      Global_Options'Elab_Body;
      E532 := E532 + 1;
      E423 := E423 + 1;
      Results_Table'Elab_Spec;
      E541 := E541 + 1;
      Task_Table'Elab_Spec;
      E567 := E567 + 1;
      Usage_Dialog_Pkg'Elab_Spec;
      E695 := E695 + 1;
      E693 := E693 + 1;
      Usage_Table'Elab_Spec;
      E569 := E569 + 1;
      File_Operations'Elab_Body;
      E502 := E502 + 1;
      Model_Operations'Elab_Body;
      E573 := E573 + 1;
      E408 := E408 + 1;
      E459 := E459 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_gmast_pt_editor");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/michael/prog/mast/pt_editor/src/binary_trees.o
   --   /home/michael/prog/mast/pt_editor/src/changes_control.o
   --   /home/michael/prog/mast/pt_editor/src/check_operations.o
   --   /home/michael/prog/mast/pt_editor/src/list_exceptions.o
   --   /home/michael/prog/mast/pt_editor/src/doubly_linked_lists.o
   --   /home/michael/prog/mast/pt_editor/src/dynamic_lists.o
   --   /home/michael/prog/mast/pt_editor/src/associations.o
   --   /home/michael/prog/mast/pt_editor/src/hash_lists.o
   --   /home/michael/prog/mast/pt_editor/src/indexed_lists.o
   --   /home/michael/prog/mast/pt_editor/src/callbacks_pt_editor.o
   --   /home/michael/prog/mast/pt_editor/src/priority_queues.o
   --   /home/michael/prog/mast/pt_editor/src/pt_editor_intl.o
   --   /home/michael/prog/mast/pt_editor/src/var_strings.o
   --   /home/michael/prog/mast/pt_editor/src/mast.o
   --   /home/michael/prog/mast/pt_editor/src/mast-tool_exceptions.o
   --   /home/michael/prog/mast/pt_editor/src/mast-annealing_parameters.o
   --   /home/michael/prog/mast/pt_editor/src/mast-hospa_parameters.o
   --   /home/michael/prog/mast/pt_editor/src/named_lists.o
   --   /home/michael/prog/mast/pt_editor/src/symbol_table.o
   --   /home/michael/prog/mast/pt_editor/src/mast_parser_tokens.o
   --   /home/michael/prog/mast/pt_editor/src/mast-io.o
   --   /home/michael/prog/mast/pt_editor/src/mast-scheduling_parameters.o
   --   /home/michael/prog/mast/pt_editor/src/mast-scheduling_policies.o
   --   /home/michael/prog/mast/pt_editor/src/mast-synchronization_parameters.o
   --   /home/michael/prog/mast/pt_editor/src/mast-graphs.o
   --   /home/michael/prog/mast/pt_editor/src/mast-processing_resources.o
   --   /home/michael/prog/mast/pt_editor/src/mast-schedulers.o
   --   /home/michael/prog/mast/pt_editor/src/mast-graphs-links.o
   --   /home/michael/prog/mast/pt_editor/src/mast-schedulers-adjustment.o
   --   /home/michael/prog/mast/pt_editor/src/mast-xmi.o
   --   /home/michael/prog/mast/pt_editor/src/mast-drivers.o
   --   /home/michael/prog/mast/pt_editor/src/mast-events.o
   --   /home/michael/prog/mast/pt_editor/src/mast-graphs-event_handlers.o
   --   /home/michael/prog/mast/pt_editor/src/mast-operations.o
   --   /home/michael/prog/mast/pt_editor/src/mast-processing_resources-network.o
   --   /home/michael/prog/mast/pt_editor/src/mast-processing_resources-processor.o
   --   /home/michael/prog/mast/pt_editor/src/mast-results.o
   --   /home/michael/prog/mast/pt_editor/src/mast-schedulers-primary.o
   --   /home/michael/prog/mast/pt_editor/src/mast-schedulers-secondary.o
   --   /home/michael/prog/mast/pt_editor/src/mast-scheduling_servers.o
   --   /home/michael/prog/mast/pt_editor/src/mast-shared_resources.o
   --   /home/michael/prog/mast/pt_editor/src/mast-systems.o
   --   /home/michael/prog/mast/pt_editor/src/mast-timers.o
   --   /home/michael/prog/mast/pt_editor/src/mast-timing_requirements.o
   --   /home/michael/prog/mast/pt_editor/src/mast-transactions.o
   --   /home/michael/prog/mast/pt_editor/src/mast-transaction_operations.o
   --   /home/michael/prog/mast/pt_editor/src/mast-consistency_checks.o
   --   /home/michael/prog/mast/pt_editor/src/mast-max_numbers.o
   --   /home/michael/prog/mast/pt_editor/src/trimmed_image.o
   --   /home/michael/prog/mast/pt_editor/src/mast-linear_translation.o
   --   /home/michael/prog/mast/pt_editor/src/mast-linear_analysis_tools.o
   --   /home/michael/prog/mast/pt_editor/src/mast-linear_task_analysis_tools.o
   --   /home/michael/prog/mast/pt_editor/src/mast-miscelaneous_tools.o
   --   /home/michael/prog/mast/pt_editor/src/mast-restrictions.o
   --   /home/michael/prog/mast/pt_editor/src/mast-tools.o
   --   /home/michael/prog/mast/pt_editor/src/mast-tools-schedulability_index.o
   --   /home/michael/prog/mast/pt_editor/src/mast-linear_priority_assignment_tools.o
   --   /home/michael/prog/mast/pt_editor/src/mast-linear_scheduling_parameters_assignment_tools.o
   --   /home/michael/prog/mast/pt_editor/src/mast-monoprocessor_tools.o
   --   /home/michael/prog/mast/pt_editor/src/filechooserdialog1_pkg-callbacks.o
   --   /home/michael/prog/mast/pt_editor/src/aboutdialog1_pkg.o
   --   /home/michael/prog/mast/pt_editor/src/dialog1_pkg-callbacks.o
   --   /home/michael/prog/mast/pt_editor/src/dialog1_pkg.o
   --   /home/michael/prog/mast/pt_editor/src/dialog_3_pkg-callbacks.o
   --   /home/michael/prog/mast/pt_editor/src/dialog_3_pkg.o
   --   /home/michael/prog/mast/pt_editor/src/dialog_yes_no_pkg-callbacks.o
   --   /home/michael/prog/mast/pt_editor/src/dialog_yes_no_pkg.o
   --   /home/michael/prog/mast/pt_editor/src/filechooserdialog1_pkg.o
   --   /home/michael/prog/mast/pt_editor/src/global_options.o
   --   /home/michael/prog/mast/pt_editor/src/mutex_table.o
   --   /home/michael/prog/mast/pt_editor/src/results_table.o
   --   /home/michael/prog/mast/pt_editor/src/task_table.o
   --   /home/michael/prog/mast/pt_editor/src/usage_dialog_pkg-callbacks.o
   --   /home/michael/prog/mast/pt_editor/src/usage_dialog_pkg.o
   --   /home/michael/prog/mast/pt_editor/src/usage_table.o
   --   /home/michael/prog/mast/pt_editor/src/file_operations.o
   --   /home/michael/prog/mast/pt_editor/src/model_operations.o
   --   /home/michael/prog/mast/pt_editor/src/pt_editor_pkg.o
   --   /home/michael/prog/mast/pt_editor/src/pt_editor_pkg-callbacks.o
   --   /home/michael/prog/mast/pt_editor/src/pt_editor.o
   --   /home/michael/prog/mast/pt_editor/src/gmast_pt_editor.o
   --   -L/home/michael/prog/mast/pt_editor/src/
   --   -L/home/michael/prog/mast/pt_editor/src/
   --   -L/home/michael/.local/share/alire/builds/gtkada_25.0.1_d3787772/97a94b98c977366ab88bcc61398b89819b47808c7bde0b5c0bf68da8797aaebd/src/lib/gtkada/static/
   --   -L/home/michael/.local/share/alire/toolchains/gnat_native_14.1.3_965c1e0e/lib/gcc/x86_64-pc-linux-gnu/14.1.0/adalib/
   --   -static
   --   -shared-libgcc
   --   -shared-libgcc
   --   -shared-libgcc
   --   -lgnat
   --   -lm
   --   -ldl
--  END Object file/option list   

end ada_main;
