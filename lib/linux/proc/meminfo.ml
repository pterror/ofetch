type meminfo = {
  memory_total : int;
  memory_free : int;
  memory_available : int;
  buffers : int;
  cached : int;
  swap_cached : int;
  active : int;
  inactive : int;
  active_anonymous : int;
  inactive_anonymous : int;
  active_file : int;
  inactive_file : int;
  unevictable : int;
  memory_locked : int;
  swap_total : int;
  swap_free : int;
  z_swap : int;
  z_swapped : int;
  dirty : int;
  writeback : int;
  anonymous_pages : int;
  mapped : int;
  shared_memory : int;
  k_reclaimable : int;
  slab : int;
  slab_reclaimable : int;
  slab_unreclaimable : int;
  kernel_stack : int;
  page_tables : int;
  secondary_page_tables : int;
  nfs_unstable : int;
  bounce : int;
  writeback_tmp : int;
  commit_limit : int;
  committed_as : int;
  vmalloc_total : int;
  vmalloc_used : int;
  vmalloc_chunk : int;
  per_cpu : int;
  hardware_corrupted : int;
  anonymous_huge_pages : int;
  shared_memory_huge_pages : int;
  shared_memory_pmd_mapped : int;
  file_huge_pages : int;
  file_pmd_mapped : int;
  cma_total : int;
  cma_free : int;
  unaccepted : int;
  huge_pages_total : int;
  huge_pages_free : int;
  huge_pages_rsvd : int;
  huge_pages_surp : int;
  huge_page_size : int;
  huge_tlb : int;
  direct_map4k : int;
  direct_map2m : int;
  direct_map1g : int;
}

let read_meminfo () =
  let initial_meminfo : meminfo =
    {
      memory_total = 0;
      memory_free = 0;
      memory_available = 0;
      buffers = 0;
      cached = 0;
      swap_cached = 0;
      active = 0;
      inactive = 0;
      active_anonymous = 0;
      inactive_anonymous = 0;
      active_file = 0;
      inactive_file = 0;
      unevictable = 0;
      memory_locked = 0;
      swap_total = 0;
      swap_free = 0;
      z_swap = 0;
      z_swapped = 0;
      dirty = 0;
      writeback = 0;
      anonymous_pages = 0;
      mapped = 0;
      shared_memory = 0;
      k_reclaimable = 0;
      slab = 0;
      slab_reclaimable = 0;
      slab_unreclaimable = 0;
      kernel_stack = 0;
      page_tables = 0;
      secondary_page_tables = 0;
      nfs_unstable = 0;
      bounce = 0;
      writeback_tmp = 0;
      commit_limit = 0;
      committed_as = 0;
      vmalloc_total = 0;
      vmalloc_used = 0;
      vmalloc_chunk = 0;
      per_cpu = 0;
      hardware_corrupted = 0;
      anonymous_huge_pages = 0;
      shared_memory_huge_pages = 0;
      shared_memory_pmd_mapped = 0;
      file_huge_pages = 0;
      file_pmd_mapped = 0;
      cma_total = 0;
      cma_free = 0;
      unaccepted = 0;
      huge_pages_total = 0;
      huge_pages_free = 0;
      huge_pages_rsvd = 0;
      huge_pages_surp = 0;
      huge_page_size = 0;
      huge_tlb = 0;
      direct_map4k = 0;
      direct_map2m = 0;
      direct_map1g = 0;
    }
  and process_meminfo line meminfo =
    let parts = String.split_on_char ':' line in
    match parts with
    | [ name; size_string ] -> (
        let _ = Str.search_forward (Str.regexp "[0-9]+") size_string 0 in
        let size = int_of_string (Str.matched_string size_string) * 1024 in
        match name with
        | "MemTotal" -> { meminfo with memory_total = size }
        | "MemFree" -> { meminfo with memory_free = size }
        | "MemAvailable" -> { meminfo with memory_available = size }
        | "Buffers" -> { meminfo with buffers = size }
        | "Cached" -> { meminfo with cached = size }
        | "SwapCached" -> { meminfo with swap_cached = size }
        | "Active" -> { meminfo with active = size }
        | "Inactive" -> { meminfo with inactive = size }
        | "Active(anon)" -> { meminfo with active = size }
        | "Inactive(anon)" -> { meminfo with inactive = size }
        | "Active(file)" -> { meminfo with active = size }
        | "Inactive(file)" -> { meminfo with inactive = size }
        | "Unevictable" -> { meminfo with unevictable = size }
        | "Mlocked" -> { meminfo with memory_locked = size }
        | "SwapTotal" -> { meminfo with swap_total = size }
        | "SwapFree" -> { meminfo with swap_free = size }
        | "Zswap" -> { meminfo with z_swap = size }
        | "Zswapped" -> { meminfo with z_swapped = size }
        | "Dirty" -> { meminfo with dirty = size }
        | "Writeback" -> { meminfo with writeback = size }
        | "AnonPages" -> { meminfo with anonymous_pages = size }
        | "Mapped" -> { meminfo with mapped = size }
        | "Shmem" -> { meminfo with shared_memory = size }
        | "KReclaimable" -> { meminfo with k_reclaimable = size }
        | "Slab" -> { meminfo with slab = size }
        | "SReclaimable" -> { meminfo with slab_reclaimable = size }
        | "SUnreclaim" -> { meminfo with slab_unreclaimable = size }
        | "KernelStack" -> { meminfo with kernel_stack = size }
        | "PageTables" -> { meminfo with page_tables = size }
        | "SecPageTables" -> { meminfo with secondary_page_tables = size }
        | "NFS_Unstable" -> { meminfo with nfs_unstable = size }
        | "Bounce" -> { meminfo with bounce = size }
        | "WritebackTmp" -> { meminfo with writeback_tmp = size }
        | "CommitLimit" -> { meminfo with commit_limit = size }
        | "Committed_AS" -> { meminfo with committed_as = size }
        | "VmallocTotal" -> { meminfo with vmalloc_total = size }
        | "VmallocUsed" -> { meminfo with vmalloc_used = size }
        | "VmallocChunk" -> { meminfo with vmalloc_chunk = size }
        | "Percpu" -> { meminfo with per_cpu = size }
        | "HardwareCorrupted" -> { meminfo with hardware_corrupted = size }
        | "AnonHugePages" -> { meminfo with anonymous_huge_pages = size }
        | "ShmemHugePages" -> { meminfo with shared_memory_huge_pages = size }
        | "ShmemPmdMapped" -> { meminfo with shared_memory_pmd_mapped = size }
        | "FileHugePages" -> { meminfo with file_huge_pages = size }
        | "FilePmdMapped" -> { meminfo with file_pmd_mapped = size }
        | "CmaTotal" -> { meminfo with cma_total = size }
        | "CmaFree" -> { meminfo with cma_free = size }
        | "Unaccepted" -> { meminfo with unaccepted = size }
        | "HugePages_Total" -> { meminfo with huge_pages_total = size }
        | "HugePages_Free" -> { meminfo with huge_pages_free = size }
        | "HugePages_Rsvd" -> { meminfo with huge_pages_rsvd = size }
        | "HugePages_Surp" -> { meminfo with huge_pages_surp = size }
        | "Hugepagesize" -> { meminfo with huge_page_size = size }
        | "Hugetlb" -> { meminfo with huge_tlb = size }
        | "DirectMap4k" -> { meminfo with direct_map4k = size }
        | "DirectMap2M" -> { meminfo with direct_map2m = size }
        | _ -> meminfo)
    | _ -> meminfo
  in
  Io.file_fold_left_lines "/proc/meminfo" process_meminfo initial_meminfo
