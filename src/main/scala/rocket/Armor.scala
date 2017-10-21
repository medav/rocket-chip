
package armor

import chisel3._
import chisel3.util._

//
// Call Convention:
// Return Register is x10, x11 (x10 for the first 8 bytes)
// Arguments are in x10 - x17
//
// Thus, Armor will track arguments in x10, x11:
//  - malloc(size) => x10 = size
//  - calloc(size, count) => x10 = size, x11 = count
//  - free(ptr) => x10 = ptr
//

//
// Range Cache has multiple operations:
//  1. Add new range (Memory allocation occured)
//  2. Clear a range (Deallocation occured)
//  3. Update an existing range (Reallocation occured)
//

class ArmorCtrl(addr_width : Int) extends Bundle {
    val reserved = UInt((addr_width-1).W)
    val armor_en = Bool()

    override def cloneType = new ArmorCtrl(addr_width).asInstanceOf[this.type]
}

object RangeCacheOperations {
    val num_ops = 4
    val num_bits = log2Ceil(num_ops)

    val noop = 0
    val add_range = 1
    val clear_range = 2
    val update_range = 3
}

class RangeCache(addr_width : Int, cache_size : Int) extends Module {
    val io = IO(new Bundle {
        val mem_addr = Input(UInt(addr_width.W))

        val operation = Input(UInt(RangeCacheOperations.num_bits.W))
        val range_low = Input(UInt(addr_width.W))
        val range_high = Input(UInt(addr_width.W))
        val update_addr = Input(UInt(addr_width.W))

        val hit = Output(Bool())
    })

    val ranges_low = Reg(init = Vec(Seq.fill(cache_size)(1.U(addr_width.W))))
    val ranges_high = Reg(init = Vec(Seq.fill(cache_size)(0.U(addr_width.W))))
    val ranges_hit = Wire(Vec(cache_size, Bool()))
    val ranges_mru = Reg(init = Vec(Seq.fill(cache_size)(false.B)))

    //
    // evict_index determines which range is next to be evicted from the table.
    // This will always be a range with its mru flag unset. update_index is the
    // entry index to use for update operations
    //

    val evict_index = Wire(UInt(log2Ceil(cache_size).W))
    val update_index = Wire(UInt(log2Ceil(cache_size).W))

    //
    // This determines if a range is added if the cache will be full. In this case,
    // all mru flags will be cleared.
    //

    val will_be_full = ((ranges_mru.asUInt() | (1.U << evict_index)) === 1.U)

    //
    // Perform operation based on what this module is told to do.
    //

    when (io.operation === RangeCacheOperations.add_range.U) {
        printf("Armor: Add Range: [0x%x, 0x%x]\n", io.range_low, io.range_high)
        ranges_low(evict_index) := io.range_low
        ranges_high(evict_index) := io.range_high
    }
    .elsewhen (io.operation === RangeCacheOperations.clear_range.U) {
        printf("Armor: Clear Range: [0x%x, 0x%x]\n", ranges_low(update_index), ranges_high(update_index))
        ranges_low(update_index) := 1.U
        ranges_high(update_index) := 0.U
        ranges_mru(update_index) := false.B
    }
    .elsewhen (io.operation === RangeCacheOperations.update_range.U) {
        printf("Armor: Update Range: [0x%x, 0x%x]\n", io.range_low, io.range_high)
        ranges_low(update_index) := io.range_low
        ranges_high(update_index) := io.range_high
    }

    //
    // Compute hit flags and next mru_index.
    //

    for (i <- 0 to cache_size-1) {
        when ((io.mem_addr >= ranges_low(i)) && (io.mem_addr <= ranges_high(i))) {
            ranges_hit(i) := true.B
            ranges_mru(i) := true.B
        }
        .otherwise {
            ranges_hit(i) := false.B
        }

        when (ranges_low(i) === io.update_addr) {
            update_index := i.U
        }
    }

    //
    // Clear the mru flags if the module will end up full with the new range.
    //
    // N.B. This doesn't have to be separate from the above loop but it's better
    //      organized this way.
    //

    for (i <- 0 to cache_size-1) {
        when (!ranges_hit(i) && will_be_full && (io.operation === RangeCacheOperations.add_range.U)) {
            ranges_mru(i) := false.B
        }
    }

    when (ranges_hit.asUInt() === 0.U) {
        io.hit := false.B
    }
    .otherwise {
        io.hit := true.B
    }
}

class MemoryCheckerCsrs(addr_width : Int) extends Bundle {
    val text_start = UInt(addr_width.W)
    val text_end = UInt(addr_width.W)

    val data_start = UInt(addr_width.W)
    val bss_end = UInt(addr_width.W)

    val stack_base = UInt(addr_width.W)

    override def cloneType = new MemoryCheckerCsrs(addr_width).asInstanceOf[this.type]
}

class MemoryChecker(addr_width : Int, cache_size : Int) extends Module {
    val io = IO(new Bundle {
        val csrs = Input(new MemoryCheckerCsrs(addr_width))

        val mem_check = Input(Bool())
        val mem_addr = Input(UInt(addr_width.W))

        val cache_operation = Input(UInt(2.W))
        val cache_range_low = Input(UInt(addr_width.W))
        val cache_range_high = Input(UInt(addr_width.W))
        val cache_update_addr = Input(UInt(addr_width.W))

        val stack_pointer = Input(UInt(addr_width.W))

        val alarm = Output(Bool())
    })

    val range_cache = Module(new RangeCache(addr_width, cache_size))
    range_cache.io.mem_addr := io.mem_addr
    range_cache.io.operation := io.cache_operation
    range_cache.io.range_low := io.cache_range_low
    range_cache.io.range_high := io.cache_range_high
    range_cache.io.update_addr := io.cache_update_addr

    val text_filtered = (io.mem_addr >= io.csrs.text_start) && (io.mem_addr <= io.csrs.text_end)
    val data_filtered = (io.mem_addr >= io.csrs.data_start) && (io.mem_addr <= io.csrs.bss_end)
    val stack_filtered = (io.mem_addr >= io.stack_pointer) && (io.mem_addr <= io.csrs.stack_base)

    val addr_filtered = text_filtered || data_filtered || stack_filtered
    val mem_addr_valid = (!addr_filtered) && io.mem_check

    when (text_filtered && io.mem_check) {
        printf(".text filtered 0x%x (text_start = 0x%x, text_end = 0x%x)\n", io.mem_addr, io.csrs.text_start, io.csrs.text_end)
    }

    when (data_filtered && io.mem_check) {
        printf(".data filtered 0x%x (data_start = 0x%x, bss_end = 0x%x)\n", io.mem_addr, io.csrs.data_start, io.csrs.bss_end)
    }

    when (stack_filtered && io.mem_check) {
        printf(".stack filtered 0x%x (sp = 0x%x, stack_base = 0x%x)\n", io.mem_addr, io.stack_pointer, io.csrs.stack_base)
    }

    io.alarm := mem_addr_valid && (!range_cache.io.hit)
}

object AllocationFunctionIds {
    val num_funcs = 6
    val num_bits = log2Ceil(num_funcs)

    val malloc = 0
    val calloc = 1
    val realloc = 2
    val free = 3
    val mmap = 4
    val munmap = 5
}

class AllocationCheckerCsrs(addr_width : Int) extends Bundle {
    val func_start_addrs = Vec(AllocationFunctionIds.num_funcs, UInt(addr_width.W))

    override def cloneType = new AllocationCheckerCsrs(addr_width).asInstanceOf[this.type]
}

class AllocationChecker(addr_width : Int) extends Module {
    val io = IO(new Bundle {
        val csrs = Input(new AllocationCheckerCsrs(addr_width))

        val inst_pc = Input(UInt(addr_width.W))
        val ret_addr = Input(UInt(addr_width.W))
        val arg0 = Input(UInt(addr_width.W))
        val arg1 = Input(UInt(addr_width.W))
        val ret = Input(UInt(addr_width.W))

        val cache_operation = Output(UInt(RangeCacheOperations.num_bits.W))
        val cache_range_low = Output(UInt(addr_width.W))
        val cache_range_high = Output(UInt(addr_width.W))
        val cache_update_addr = Output(UInt(addr_width.W))
    })

    val cache_operation = Wire(UInt(RangeCacheOperations.num_bits.W))
    val cur_func_id = Wire(UInt(AllocationFunctionIds.num_bits.W))
    val latched_func_id = Reg(UInt(AllocationFunctionIds.num_bits.W))
    val func_start_hit = Wire(Vec(AllocationFunctionIds.num_funcs, Bool()))
    val func_end_hit = Wire(Vec(AllocationFunctionIds.num_funcs, Bool()))
    val ret_addr_latched = Reg(init = ~0.U(addr_width.W))

    cur_func_id := PriorityEncoder(func_start_hit)

    //
    // Compute flags telling whether the start or end of each function
    // in the table is hit. This module assumes a consistent state for
    // the function table, meaning there are aren't mismatched addresses, 
    // etc...
    //

    for (i <- 0 until AllocationFunctionIds.num_funcs) {
        when (io.inst_pc === io.csrs.func_start_addrs(i)) {
            func_start_hit(i) := true.B
        }
        .otherwise {
            func_start_hit(i) := false.B
        }
    }

    val arg0_latched = Reg(UInt(addr_width.W))
    val arg1_latched = Reg(UInt(addr_width.W))

    //
    // For all functions, arg0 and arg1 are always recorded on the
    // start of the function.
    //

    when (func_start_hit.asUInt() != 0.U) {
        printf("Armor: Start capture on funcid = %d\n", cur_func_id)
        arg0_latched := io.arg0
        arg1_latched := io.arg1
        latched_func_id := cur_func_id
        ret_addr_latched := io.ret_addr
    }

    //
    // Compute range bounds and output a cache operation depending on
    // what function just completed.
    //

    when (io.inst_pc === ret_addr_latched) {
        printf("Armor: End capture on funcid = %d\n", latched_func_id)
        when (latched_func_id === AllocationFunctionIds.malloc.U) {
            // range_low = malloc(length)
            // range_high = range_low + length
            cache_operation := RangeCacheOperations.add_range.U
            io.cache_range_low := io.ret
            io.cache_range_high := io.ret + arg0_latched
        }
        .elsewhen (latched_func_id === AllocationFunctionIds.calloc.U) {
            // range_low = calloc(count, size)
            // range_high = range_low + count * size
            cache_operation := RangeCacheOperations.add_range.U
            io.cache_range_low := io.ret
            io.cache_range_high := io.ret + arg0_latched * arg1_latched
        }
        .elsewhen (latched_func_id === AllocationFunctionIds.realloc.U) {
            // range_low = realloc(orig_low, new_length)
            // range_high = range_low + new_length
            cache_operation := RangeCacheOperations.update_range.U
            io.cache_range_low := io.ret
            io.cache_range_high := io.ret + arg1_latched
            io.cache_update_addr := arg0_latched
        }
        .elsewhen (latched_func_id === AllocationFunctionIds.free.U) {
            cache_operation := RangeCacheOperations.clear_range.U
            io.cache_update_addr := arg0_latched
        }
        .elsewhen (latched_func_id === AllocationFunctionIds.mmap.U) {
            cache_operation := RangeCacheOperations.add_range.U
            io.cache_range_low := io.ret
            io.cache_range_high := io.ret + arg1_latched
        }
        .elsewhen (latched_func_id === AllocationFunctionIds.free.U) {
            cache_operation := RangeCacheOperations.clear_range.U
            io.cache_update_addr := arg0_latched
        }

        ret_addr_latched := ~0.U(addr_width.W)

        when ((io.ret != 0.U) || (latched_func_id === AllocationFunctionIds.free.U) || (latched_func_id === AllocationFunctionIds.munmap.U)) {
            io.cache_operation := cache_operation
        }
        .otherwise {
            io.cache_operation := RangeCacheOperations.noop.U
        }
    }
    .otherwise {
        io.cache_operation := RangeCacheOperations.noop.U
    }
}



// class ArmorMmioCsrModule(mmio_base : Int, addr_width : Int) extends Module {
//     val io = IO(new Bundle {
//         val read_addr = Input(UInt(addr_width.W))
//         val read_data = Output(UInt(addr_width.W))
//         val claim_read = Output(Bool())

//         val write_addr = Input(UInt(addr_width.W))
//         val write_data = Input(UInt(addr_width.W))
//         val write = Input(Bool())
//         val claim_write = Output(Bool())

//         
//         val allocation_checker_csrs = Output(new AllocationCheckerCsrs(addr_width))
//     })



//     val read_offset = io.read_addr - mmio_base.U
//     val write_offset = io.write_addr - mmio_base.U

//     val claim_read = (io.read_addr >= mmio_base.U) && (io.read_addr <= (mmio_base + 0x1000).U)
//     io.claim_read := claim_read

//     val claim_write = (io.write_addr >= mmio_base.U) && (io.write_addr <= (mmio_base + 0x1000).U)
//     io.claim_write := claim_write

//     def MapRegister(reg : UInt, map_offset : Int) {
//         when (claim_read && (read_offset === map_offset.U)) {
//             io.read_data := reg
//         }
        
//         when (claim_write && (write_offset === map_offset.U) && io.write) {
//             reg := io.write_data
//         }
//     }

//     //
//     // Memory Checker Control/Status Registers. Start at 0x0100
//     //

//     MapRegister(memory_checker_csrs.text_start, 0x0100)
//     MapRegister(memory_checker_csrs.text_end, 0x0108)
//     MapRegister(memory_checker_csrs.data_start, 0x0110)
//     MapRegister(memory_checker_csrs.bss_end, 0x0118)
//     MapRegister(memory_checker_csrs.stack_base, 0x0120)

//     //
//     // Function Table Start/End Addresses. Start at 0x0200
//     //

//     for (i <- 0 until AllocationFunctionIds.num_funcs) {
//         MapRegister(allocation_checker_csrs.func_start_addrs(i), 0x200 + i*16)
//         MapRegister(allocation_checker_csrs.func_end_addrs(i), 0x208 + i*16)
//     }
    
// }
