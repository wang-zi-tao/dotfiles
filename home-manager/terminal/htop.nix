{ config, pkgs, lib, ... }: {
  programs.htop = {
    enable = true;
    settings = {
      htop_version = "3.1.1";
      config_reader_min_version = 2;
      sort_key = 46;
      sort_direction = -1;
      tree_sort_key = 0;
      tree_sort_direction = 1;
      hide_kernel_threads = 1;
      hide_userland_threads = 1;
      shadow_other_users = 0;
      show_thread_names = 1;
      show_program_path = 1;
      highlight_base_name = 1;
      highlight_deleted_exe = 1;
      highlight_megabytes = 1;
      highlight_threads = 1;
      highlight_changes = 0;
      highlight_changes_delay_secs = 5;
      find_comm_in_cmdline = 1;
      strip_exe_from_cmdline = 1;
      show_merged_command = 1;
      tree_view = 0;
      tree_view_always_by_pid = 0;
      all_branches_collapsed = 0;
      header_margin = 1;
      detailed_cpu_time = 0;
      cpu_count_from_one = 1;
      show_cpu_usage = 1;
      show_cpu_frequency = 0;
      show_cpu_temperature = 0;
      degree_fahrenheit = 0;
      update_process_names = 0;
      account_guest_in_cpu_meter = 0;
      color_scheme = 5;
      enable_mouse = 1;
      delay = 15;
      hide_function_bar = 0;
      header_layout = "two_50_50";
      fields = with config.lib.htop.fields; [
        PID
        123
        USER
        STATE
        M_SIZE
        M_RESIDENT
        40
        PERCENT_CPU
        PERCENT_MEM
        IO_READ_RATE
        COMM
      ];
    } // (with config.lib.htop;
      leftMeters [
        (bar "LeftCPUs4")
        (bar "MemorySwap")
        (text "NetworkIO")
        (text "DiskIO")
        (text "PressureStallIOFull")
      ]) // (with config.lib.htop;
        rightMeters [
          (bar "RightCPUs4")
          (text "Tasks")
          (text "LoadAverage")
          (text "Systemd")
          (text "Battery")
        ]);
  };
}
