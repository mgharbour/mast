 
ME_Processing_Resource Me_Regular_Processor cpu_1 Proc_Res_Canvas 360 465 
ME_Processing_Resource Me_Regular_Processor cpu_2 Proc_Res_Canvas 40 465 
ME_Processing_Resource Me_Packet_Based_Network red.1 Proc_Res_Canvas 360 380 
ME_Processing_Resource Me_Packet_Based_Network red.2 Proc_Res_Canvas 520 295 
ME_Processing_Resource Me_Packet_Based_Network red.3 Proc_Res_Canvas 40 295 
ME_Processing_Resource Me_Packet_Based_Network red.22 Proc_Res_Canvas 520 210 
ME_Timer Me_System_Timer cpu_1 Proc_Res_Canvas 200 465 
ME_Timer Me_System_Timer cpu_2 Proc_Res_Canvas 520 380 
ME_Driver Me_Packet_Driver cpu_1,red.1 Proc_Res_Canvas 204 326 
ME_Driver Me_Packet_Driver cpu_2,red.1 Proc_Res_Canvas 162 390 
ME_Driver Me_Packet_Driver cpu_1,red.2 Proc_Res_Canvas 596 459 
ME_Driver Me_Packet_Driver cpu_2,red.2 Proc_Res_Canvas 754 539 
ME_Scheduler Me_Primary_Scheduler cpu_1 Proc_Res_Canvas 360 210 
ME_Scheduler Me_Primary_Scheduler cpu_2 Proc_Res_Canvas 200 210 
ME_Scheduler Me_Primary_Scheduler red.1 Proc_Res_Canvas 40 210 
ME_Scheduler Me_Primary_Scheduler red.2 Proc_Res_Canvas 520 125 
ME_Scheduler Me_Primary_Scheduler red.22 Proc_Res_Canvas 360 125 
ME_Scheduler Me_Secondary_Scheduler edf_scheduler_1 Proc_Res_Canvas 200 125 
ME_Scheduler Me_Secondary_Scheduler net_scheduler Proc_Res_Canvas 520 40 
ME_Scheduler Me_Secondary_Scheduler edf_scheduler_2 Proc_Res_Canvas 200 40 
ME_Scheduler Me_Primary_Scheduler cpu_1 Sched_Server_Canvas 360 380 
ME_Scheduler Me_Primary_Scheduler cpu_2 Sched_Server_Canvas 520 295 
ME_Scheduler Me_Primary_Scheduler red.2 Sched_Server_Canvas 200 210 
ME_Scheduler Me_Secondary_Scheduler edf_scheduler_1 Sched_Server_Canvas 520 125 
ME_Scheduler Me_Secondary_Scheduler edf_scheduler_2 Sched_Server_Canvas 200 125 
ME_Scheduling_Server Me_Server rs232_c_server1 Sched_Server_Canvas 520 380 
ME_Scheduling_Server Me_Server rs232_p_server1 Sched_Server_Canvas 200 380 
ME_Scheduling_Server Me_Server rtep_internal_thread2 Sched_Server_Canvas 40 380 
ME_Scheduling_Server Me_Server rtep_isr2 Sched_Server_Canvas 360 295 
ME_Scheduling_Server Me_Server ethernet_server1 Sched_Server_Canvas 200 295 
ME_Scheduling_Server Me_Server ethernet_server2 Sched_Server_Canvas 40 295 
ME_Scheduling_Server Me_Server edf_scheduler_1 Sched_Server_Canvas 520 210 
ME_Scheduling_Server Me_Server net_scheduler Sched_Server_Canvas 360 210 
ME_Scheduling_Server Me_Server edf_scheduler_2 Sched_Server_Canvas 40 210 
ME_Scheduling_Server Me_Server edf_task Sched_Server_Canvas 360 125 
ME_Scheduling_Server Me_Server tarea_1 Sched_Server_Canvas 40 125 
ME_Scheduling_Server Me_Server tarea_11 Sched_Server_Canvas 520 40 
ME_Scheduling_Server Me_Server tarea_x Sched_Server_Canvas 360 40 
ME_Scheduling_Server Me_Server tarea_2 Sched_Server_Canvas 200 40 
ME_Scheduling_Server Me_Server tarea_3 Sched_Server_Canvas 40 40 
ME_Scheduling_Server Me_Server edf_scheduler_1 Proc_Res_Canvas 40 125 
ME_Scheduling_Server Me_Server net_scheduler Proc_Res_Canvas 360 40 
ME_Scheduling_Server Me_Server edf_scheduler_2 Proc_Res_Canvas 40 40 
ME_Shared_Resource Me_Immediate_Ceiling_Resource dato_pcp_1 Shared_Res_Canvas 40 125 
ME_Shared_Resource Me_Immediate_Ceiling_Resource dato_pcp_2 Shared_Res_Canvas 520 40 
ME_Shared_Resource Me_Priority_Inheritance_Resource dato_pi_1 Shared_Res_Canvas 360 40 
ME_Shared_Resource Me_Priority_Inheritance_Resource dato_pi_2 Shared_Res_Canvas 200 40 
ME_Shared_Resource Me_SRP_Resource dato_srp_1 Shared_Res_Canvas 40 40 
ME_Operation Me_Simple_Operation rs232_p_send Operation_Canvas 40 465 
ME_Operation Me_Simple_Operation rs232_p_receive Operation_Canvas 520 380 
ME_Operation Me_Simple_Operation rs232_c_send Operation_Canvas 360 380 
ME_Operation Me_Simple_Operation rs232_c_receive Operation_Canvas 200 380 
ME_Operation Me_Simple_Operation rtep_send Operation_Canvas 40 380 
ME_Operation Me_Simple_Operation rtep_receive Operation_Canvas 520 295 
ME_Operation Me_Simple_Operation rtep_isr_op Operation_Canvas 360 295 
ME_Operation Me_Simple_Operation tco Operation_Canvas 200 295 
ME_Operation Me_Simple_Operation tmo Operation_Canvas 40 295 
ME_Operation Me_Simple_Operation pdo Operation_Canvas 520 210 
ME_Operation Me_Simple_Operation tro Operation_Canvas 360 210 
ME_Operation Me_Simple_Operation pro Operation_Canvas 200 210 
ME_Operation Me_Simple_Operation ethernet_send Operation_Canvas 40 210 
ME_Operation Me_Simple_Operation ethernet_receive Operation_Canvas 520 125 
ME_Operation Me_Simple_Operation ethernet_send1 Operation_Canvas 360 125 
ME_Operation Me_Simple_Operation ethernet_receive1 Operation_Canvas 200 125 
ME_Operation Me_Simple_Operation proc_1 Operation_Canvas 40 125 
ME_Operation Me_Simple_Operation proc_2 Operation_Canvas 520 40 
ME_Operation Me_Simple_Operation proc_3 Operation_Canvas 360 40 
ME_Operation Me_Composite_Operation proc_4 Operation_Canvas 200 40 
ME_Operation Me_Simple_Operation mensaje_1 Operation_Canvas 40 40 
ME_Transaction Me_Regular_Transaction trans_1 Transaction_Canvas 200 40 
ME_Transaction Me_Regular_Transaction trans_2 Transaction_Canvas 40 40 
ME_Link Me_External_Link ep1,trans_1 trans_1 85 515 
ME_Link Me_External_Link es1,trans_1 trans_1 85 400 
ME_Link Me_External_Link eu1,trans_1 trans_1 85 170 
ME_Link Me_External_Link eb1,trans_1 trans_1 85 55 
ME_Link Me_Internal_Link an1,trans_1 trans_1 1605 170 
ME_Link Me_Internal_Link an2,trans_1 trans_1 1605 55 
ME_Link Me_Internal_Link an3,trans_1 trans_1 845 630 
ME_Link Me_Internal_Link an4,trans_1 trans_1 1225 285 
ME_Link Me_Internal_Link an5,trans_1 trans_1 845 515 
ME_Link Me_Internal_Link an6,trans_1 trans_1 845 285 
ME_Link Me_Internal_Link an7,trans_1 trans_1 1225 515 
ME_Link Me_Internal_Link an8,trans_1 trans_1 1225 170 
ME_Link Me_Internal_Link an9,trans_1 trans_1 1225 400 
ME_Link Me_Internal_Link an10,trans_1 trans_1 1225 55 
ME_Link Me_Internal_Link i1,trans_1 trans_1 465 400 
ME_Link Me_Internal_Link i2,trans_1 trans_1 465 55 
ME_Link Me_Internal_Link i3,trans_1 trans_1 845 400 
ME_Link Me_Internal_Link ai1,trans_1 trans_1 845 170 
ME_Link Me_Internal_Link ai2,trans_1 trans_1 845 55 
ME_Link Me_Internal_Link ao1,trans_1 trans_1 1225 630 
ME_Link Me_Internal_Link ao2,trans_1 trans_1 1605 285 
ME_Link Me_External_Link entrada,trans_2 trans_2 85 55 
ME_Link Me_Internal_Link salida,trans_2 trans_2 465 55 
ME_Event_Handler Me_Simple_Event_Handler 1,trans_1 trans_1 1370 155 
ME_Event_Handler Me_Simple_Event_Handler 2,trans_1 trans_1 1370 40 
ME_Event_Handler Me_Simple_Event_Handler 3,trans_1 trans_1 990 500 
ME_Event_Handler Me_Simple_Event_Handler 4,trans_1 trans_1 1370 270 
ME_Event_Handler Me_Simple_Event_Handler 5,trans_1 trans_1 990 270 
ME_Event_Handler Me_Simple_Event_Handler 6,trans_1 trans_1 990 155 
ME_Event_Handler Me_Simple_Event_Handler 7,trans_1 trans_1 990 40 
ME_Event_Handler Me_Multi_Input_Event_Handler 8,trans_1 trans_1 230 385 
ME_Event_Handler Me_Multi_Input_Event_Handler 9,trans_1 trans_1 230 40 
ME_Event_Handler Me_Multi_Output_Event_Handler 10,trans_1 trans_1 610 385 
ME_Event_Handler Me_Multi_Output_Event_Handler 11,trans_1 trans_1 610 40 
ME_Event_Handler Me_Multi_Output_Event_Handler 12,trans_1 trans_1 990 385 
ME_Event_Handler Me_Simple_Event_Handler 1,trans_2 trans_2 230 40 