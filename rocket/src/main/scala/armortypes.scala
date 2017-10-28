
package armor

import Chisel._

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
    val reserved = UInt(width = (addr_width-1))
    val armor_en = Bool()

    override def cloneType = new ArmorCtrl(addr_width).asInstanceOf[this.type]
}

object RangeCacheOperations {
    val num_ops = 4
    val num_bits = log2Up(num_ops)

    val noop = 0
    
    val add_range = 1
    val clear_range = 2
    val update_range = 3

    // val clear_cache = 7
}

class MemoryCheckerCsrs(addr_width : Int) extends Bundle {
    val text_start = UInt(width = addr_width)
    val text_end = UInt(width = addr_width)

    val data_start = UInt(width = addr_width)
    val bss_end = UInt(width = addr_width)

    val stack_base = UInt(width = addr_width)

    override def cloneType = new MemoryCheckerCsrs(addr_width).asInstanceOf[this.type]
}

object AllocationFunctionIds {
    val num_funcs = 6
    val num_bits = log2Up(num_funcs)

    val malloc = 0
    val calloc = 1
    val realloc = 2
    val free = 3
    val mmap = 4
    val munmap = 5
}

class AllocationCheckerCsrs(addr_width : Int) extends Bundle {
    val func_start_addrs = Vec(AllocationFunctionIds.num_funcs, UInt(width = addr_width))

    override def cloneType = new AllocationCheckerCsrs(addr_width).asInstanceOf[this.type]
}

