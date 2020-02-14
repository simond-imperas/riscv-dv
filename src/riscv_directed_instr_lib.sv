/*
 * Copyright 2018 Google LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// Base class for directed instruction stream
class riscv_directed_instr_stream extends riscv_rand_instr_stream;

  `uvm_object_utils(riscv_directed_instr_stream)

  string label;

  function new(string name = "");
    super.new(name);
  endfunction

  function void post_randomize();
    foreach(instr_list[i]) begin
      instr_list[i].has_label = 1'b0;
      instr_list[i].atomic = 1'b1;
    end
    instr_list[0].comment = $sformatf("Start %0s", get_name());
    instr_list[$].comment = $sformatf("End %0s", get_name());
    if(label!= "") begin
      instr_list[0].label = label;
      instr_list[0].has_label = 1'b1;
    end
  endfunction

endclass

// Base class for memory access stream
class riscv_mem_access_stream extends riscv_directed_instr_stream;

  int             max_data_page_id;
  mem_region_t    data_page[$];

  `uvm_object_utils(riscv_mem_access_stream)
  `uvm_object_new

  function void pre_randomize();
    if(kernel_mode) begin
      data_page = cfg.s_mem_region;
    end else begin
      data_page = cfg.mem_region;
    end
    max_data_page_id = data_page.size();
  endfunction

  // Use "la" instruction to initialize the base regiseter
  virtual function void add_rs1_init_la_instr(riscv_reg_t gpr, int id, int base = 0);
    riscv_pseudo_instr la_instr;
    la_instr = riscv_pseudo_instr::type_id::create("la_instr");
    la_instr.pseudo_instr_name = LA;
    la_instr.rd = gpr;
    if(kernel_mode) begin
      la_instr.imm_str = $sformatf("%s+%0d", cfg.s_mem_region[id].name, base);
    end else begin
      la_instr.imm_str = $sformatf("%s+%0d", cfg.mem_region[id].name, base);
    end
    instr_list.push_front(la_instr);
  endfunction

  // Insert some other instructions to mix with mem_access instruction
  virtual function void add_mixed_instr(int instr_cnt);
    riscv_instr_base instr;
    setup_allowed_instr(1, 1);
    for(int i = 0; i < instr_cnt; i ++) begin
      instr = riscv_instr_base::type_id::create("instr");
      randomize_instr(instr);
      insert_instr(instr);
    end
  endfunction

endclass

// Jump instruction (JAL, JALR)
// la rd0, jump_tagert_label
// addi rd1, offset, rd0
// jalr rd, offset, rd1
// For JAL, restore the stack before doing the jump

class riscv_jump_instr extends riscv_directed_instr_stream;

  riscv_instr_base     jump;
  riscv_instr_base     addi;
  riscv_pseudo_instr   la;
  riscv_instr_base     branch;
  rand riscv_reg_t     gpr;
  rand int             imm;
  rand bit             enable_branch;
  rand int             mixed_instr_cnt;
  riscv_instr_base     stack_exit_instr[];
  string               target_program_label;
  int                  idx;
  bit                  use_jalr;

  constraint instr_c {
    !(gpr inside {cfg.reserved_regs, ZERO});
    imm inside {[-1023:1023]};
    mixed_instr_cnt inside {[5:10]};
  }

  `uvm_object_utils(riscv_jump_instr)

  function new(string name = "");
    super.new(name);
    jump = riscv_instr_base::type_id::create("jump");
    la = riscv_pseudo_instr::type_id::create("la");
    addi = riscv_instr_base::type_id::create("addi");
    branch = riscv_instr_base::type_id::create("branch");
  endfunction

  function void post_randomize();
    riscv_instr_base instr[];
    `DV_CHECK_RANDOMIZE_WITH_FATAL(jump,
      (use_jalr) -> (instr_name == JALR);
      instr_name dist {JAL := 2, JALR := 6, C_JALR := 2};
      if (cfg.disable_compressed_instr || (cfg.ra != RA)) {
        instr_name != C_JALR;
      }
      rd == cfg.ra;
      rs1 == gpr;
    )
    `DV_CHECK_RANDOMIZE_WITH_FATAL(addi,
      rs1 == gpr;
      instr_name == ADDI;
      rd  == gpr;
    )
    `DV_CHECK_RANDOMIZE_WITH_FATAL(branch,
      instr_name inside {BEQ, BNE, BLT, BGE, BLTU, BGEU};)
    la.pseudo_instr_name = LA;
    la.imm_str = target_program_label;
    la.rd = gpr;
    // Generate some random instructions to mix with jump instructions
    reserved_rd = {gpr};
    initialize_instr_list(mixed_instr_cnt);
    gen_instr(1'b1);
    addi.imm_str = $sformatf("%0d", imm);
    jump.imm_str = $sformatf("%0d", -imm);
    // The branch instruction is always inserted right before the jump instruction to avoid
    // skipping other required instructions like restore stack, load jump base etc.
    // The purse of adding the branch instruction here is to cover branch -> jump scenario.
    if(enable_branch) instr = {branch};
    // Restore stack before unconditional jump
    if(jump.rd == ZERO) begin
      instr= {stack_exit_instr, instr};
    end
    if(jump.instr_name == JAL) begin
      jump.imm_str = target_program_label;
    end else if (jump.instr_name == C_JALR) begin
      instr = {la, instr};
    end else begin
      instr = {la, addi, instr};
    end
    mix_instr_stream(instr);
    instr_list = {instr_list, jump};
    foreach(instr_list[i]) begin
      instr_list[i].has_label = 1'b0;
      instr_list[i].atomic = 1'b1;
    end
    jump.has_label = 1'b1;
    jump.label = $sformatf("j_%0s_%0s_%0d", label, target_program_label, idx);
    branch.imm_str = jump.label;
    branch.comment = "branch to jump instr";
    branch.branch_assigned = 1'b1;
  endfunction
endclass

// Stress back to back jump instruction
class riscv_jal_instr extends riscv_rand_instr_stream;

  riscv_instr_base     jump[];
  riscv_instr_base     jump_start;
  riscv_instr_base     jump_end;
  rand int unsigned    num_of_jump_instr;
  riscv_instr_name_t   jal[$];

  constraint instr_c {
    num_of_jump_instr inside {[10:30]};
  }

  `uvm_object_utils(riscv_jal_instr)

  function new(string name = "");
    super.new(name);
  endfunction

  function void post_randomize();
    int order[];
    order = new[num_of_jump_instr];
    jump = new[num_of_jump_instr];
    foreach (order[i]) begin
      order[i] = i;
    end
    order.shuffle();
    setup_allowed_instr(1, 1);
    jal = {JAL};
    if (!cfg.disable_compressed_instr) begin
      jal.push_back(C_J);
      if (XLEN == 32) begin
        jal.push_back(C_JAL);
      end
    end
    // First instruction
    jump_start = riscv_instr_base::type_id::create("jump_start");
    `DV_CHECK_RANDOMIZE_WITH_FATAL(jump_start,
      instr_name == JAL;
      rd == cfg.ra;
    )
    jump_start.imm_str = $sformatf("%0df", order[0]);
    jump_start.label = label;
    // Last instruction
    jump_end = riscv_instr_base::type_id::create("jump_end");
    randomize_instr(jump_end);
    jump_end.label = $sformatf("%0d", num_of_jump_instr);
    foreach (jump[i]) begin
      jump[i] = riscv_instr_base::type_id::create($sformatf("jump_%0d", i));
      `DV_CHECK_RANDOMIZE_WITH_FATAL(jump[i],
        instr_name inside {jal};
        rd dist {RA := 5, T1 := 2, [SP:T0] :/ 1, [T2:T6] :/ 2};
        !(rd inside {cfg.reserved_regs});
      )
      jump[i].label = $sformatf("%0d", i);
    end
    foreach (order[i]) begin
      if (i == num_of_jump_instr - 1) begin
        jump[order[i]].imm_str = $sformatf("%0df", num_of_jump_instr);
      end else begin
        if (order[i+1] > order[i]) begin
          jump[order[i]].imm_str = $sformatf("%0df", order[i+1]);
        end else begin
          jump[order[i]].imm_str = $sformatf("%0db", order[i+1]);
        end
      end
    end
    instr_list = {jump_start, jump, jump_end};
    foreach (instr_list[i]) begin
      instr_list[i].has_label = 1'b1;
      instr_list[i].atomic = 1'b1;
    end
  endfunction
endclass

// Push stack instruction stream
class riscv_push_stack_instr extends riscv_rand_instr_stream;

  int                      stack_len;
  int                      num_of_reg_to_save;
  int                      num_of_redudant_instr;
  riscv_instr_base         push_stack_instr[];
  riscv_reg_t              saved_regs[];
  rand riscv_rand_instr    branch_instr;
  rand bit                 enable_branch;
  string                   push_start_label;

  `uvm_object_utils(riscv_push_stack_instr)

  function new(string name = "");
    super.new(name);
  endfunction

  function void init();
    // Save RA, T0
    reserved_rd = {cfg.ra};
    saved_regs = {cfg.ra};
    num_of_reg_to_save = saved_regs.size();
    if(num_of_reg_to_save * (XLEN/8) > stack_len) begin
      `uvm_fatal(get_full_name(), $sformatf("stack len [%0d] is not enough to store %d regs",
                 stack_len, num_of_reg_to_save))
    end
    num_of_redudant_instr = $urandom_range(3,10);
    initialize_instr_list(num_of_redudant_instr);
  endfunction

  virtual function void gen_push_stack_instr(int stack_len, bit allow_branch = 1);
    this.stack_len = stack_len;
    init();
    gen_instr(1'b1);
    push_stack_instr = new[num_of_reg_to_save+1];
    foreach(push_stack_instr[i]) begin
      push_stack_instr[i] = riscv_instr_base::type_id::
                             create($sformatf("push_stack_instr_%0d", i));
    end
    // addi sp,sp,-imm
    `DV_CHECK_RANDOMIZE_WITH_FATAL(push_stack_instr[0],
                                   instr_name == ADDI; rd == cfg.sp; rs1 == cfg.sp;
                                   imm == (~stack_len + 1);)
    push_stack_instr[0].imm_str = $sformatf("-%0d", stack_len);
    foreach(saved_regs[i]) begin
      if(XLEN == 32) begin
        `DV_CHECK_RANDOMIZE_WITH_FATAL(push_stack_instr[i+1],
          instr_name == SW; rs2 == saved_regs[i]; rs1 == cfg.sp; imm == 4 * (i+1);)
      end else begin
        `DV_CHECK_RANDOMIZE_WITH_FATAL(push_stack_instr[i+1],
          instr_name == SD; rs2 == saved_regs[i]; rs1 == cfg.sp; imm == 8 * (i+1);)
      end
      push_stack_instr[i+1].process_load_store = 0;
    end
    if (allow_branch) begin
      `DV_CHECK_STD_RANDOMIZE_FATAL(enable_branch)
    end else begin
      enable_branch = 0;
    end
    if(enable_branch) begin
      // Cover jal -> branch scenario, the branch is added before push stack operation
      branch_instr = riscv_rand_instr::type_id::create("branch_instr");
      branch_instr.cfg = cfg;
      `ifdef DSIM
        `DV_CHECK_RANDOMIZE_WITH_FATAL(branch_instr,
                                       instr_name inside {[BEQ:BGEU], C_BEQZ, C_BNEZ};)
      `else
        `DV_CHECK_RANDOMIZE_WITH_FATAL(branch_instr, category == BRANCH;)
      `endif
      branch_instr.imm_str = push_start_label;
      branch_instr.branch_assigned = 1'b1;
      push_stack_instr[0].label = push_start_label;
      push_stack_instr[0].has_label = 1'b1;
      push_stack_instr = {branch_instr, push_stack_instr};
    end
    mix_instr_stream(push_stack_instr);
    foreach(instr_list[i]) begin
      instr_list[i].atomic = 1'b1;
      if(instr_list[i].label == "")
        instr_list[i].has_label = 1'b0;
    end
  endfunction

endclass

// Pop stack instruction stream
class riscv_pop_stack_instr extends riscv_rand_instr_stream;

  int                      stack_len;
  int                      num_of_reg_to_save;
  int                      num_of_redudant_instr;
  riscv_instr_base         pop_stack_instr[];
  riscv_reg_t              saved_regs[];

  `uvm_object_utils(riscv_pop_stack_instr)

  function new(string name = "");
    super.new(name);
  endfunction

  function void init();
    reserved_rd = {cfg.ra};
    num_of_reg_to_save = saved_regs.size();
    if(num_of_reg_to_save * 4 > stack_len) begin
      `uvm_fatal(get_full_name(), $sformatf("stack len [%0d] is not enough to store %d regs",
                 stack_len, num_of_reg_to_save))
    end
    num_of_redudant_instr = $urandom_range(3,10);
    initialize_instr_list(num_of_redudant_instr);
  endfunction

  virtual function void gen_pop_stack_instr(int stack_len, riscv_reg_t saved_regs[]);
    this.stack_len = stack_len;
    this.saved_regs = saved_regs;
    init();
    gen_instr(1'b1);
    pop_stack_instr = new[num_of_reg_to_save+1];
    foreach(pop_stack_instr[i]) begin
      pop_stack_instr[i] = riscv_instr_base::type_id::
                             create($sformatf("pop_stack_instr_%0d", i));
    end
    foreach(saved_regs[i]) begin
      if(XLEN == 32) begin
        `DV_CHECK_RANDOMIZE_WITH_FATAL(pop_stack_instr[i],
          instr_name == LW; rd == saved_regs[i]; rs1 == cfg.sp; imm == 4 * (i+1);)
      end else begin
        `DV_CHECK_RANDOMIZE_WITH_FATAL(pop_stack_instr[i],
          instr_name == LD; rd == saved_regs[i]; rs1 == cfg.sp; imm == 8 * (i+1);)
      end
      pop_stack_instr[i].process_load_store = 0;
    end
    // addi sp,sp,imm
    `DV_CHECK_RANDOMIZE_WITH_FATAL(pop_stack_instr[num_of_reg_to_save],
      instr_name == ADDI; rd == cfg.sp; rs1 == cfg.sp; imm == stack_len;)
    pop_stack_instr[num_of_reg_to_save].imm_str = $sformatf("%0d", stack_len);
    mix_instr_stream(pop_stack_instr);
    foreach(instr_list[i]) begin
      instr_list[i].atomic = 1'b1;
      instr_list[i].has_label = 1'b0;
    end
  endfunction

endclass

// Cover the long fprward and backward jump
class riscv_long_branch_instr extends riscv_rand_instr_stream;

  int branch_instr_stream_len = 100;
  int branch_instr_offset = 999;
  riscv_rand_instr_stream forward_branch_instr_stream;
  riscv_rand_instr_stream backward_branch_instr_stream;
  riscv_instr_base        jump_instr;

  `uvm_object_utils(riscv_long_branch_instr)

  function new(string name = "");
    super.new(name);
    forward_branch_instr_stream = riscv_rand_instr_stream::type_id::
                                  create("forward_branch_instr_stream");
    backward_branch_instr_stream = riscv_rand_instr_stream::type_id::
                                  create("backward_branch_instr_stream");
    jump_instr = riscv_instr_base::type_id::create("jump_instr");
  endfunction

  function void init(int instr_len);
    branch_instr_stream_len = instr_len;
    initialize_instr_list(branch_instr_offset-branch_instr_stream_len);
    forward_branch_instr_stream.cfg = cfg;
    backward_branch_instr_stream.cfg = cfg;
    forward_branch_instr_stream.initialize_instr_list(branch_instr_stream_len);
    backward_branch_instr_stream.initialize_instr_list(branch_instr_stream_len);
  endfunction

  virtual function void gen_instr(bit no_branch = 1'b0, bit no_load_store = 1'b1,
                                  bit is_debug_program = 1'b0);
    int branch_offset;
    super.gen_instr(1'b1);
    forward_branch_instr_stream.gen_instr();
    backward_branch_instr_stream.gen_instr();
    `DV_CHECK_RANDOMIZE_WITH_FATAL(jump_instr, instr_name == JAL;)
    jump_instr.imm_str = "test_done";
    instr_list = {forward_branch_instr_stream.instr_list, instr_list,
                  jump_instr, backward_branch_instr_stream.instr_list};
    foreach(instr_list[i]) begin
      instr_list[i].atomic = 1'b1;
      if(!instr_list[i].is_branch_target) begin
        instr_list[i].has_label = 1'b0;
      end
      if(instr_list[i].category == BRANCH) begin
        if(i < branch_instr_stream_len)
          branch_offset = branch_instr_offset;
        else
          branch_offset = -branch_instr_offset;
        instr_list[i].imm_str = $sformatf("target_%0d", i);
        instr_list[i].branch_assigned = 1'b1;
        // Avoid dead loop
        if(((instr_list[i+branch_offset].category == BRANCH) ||
             instr_list[i+branch_offset].is_branch_target) && (branch_offset < 0))
          branch_offset = branch_offset + 1;
        `uvm_info(get_full_name(), $sformatf("Branch [%0d] %0s -> [%0d] %0s", i,
                  instr_list[i].convert2asm(), i+branch_offset,
                  instr_list[i+branch_offset].convert2asm()), UVM_LOW)
        if(i < -branch_offset)
          `uvm_fatal(get_name(), $sformatf("Unexpected branch instr at %0d", i))
        instr_list[i+branch_offset].label = $sformatf("target_%0d", i);
        instr_list[i+branch_offset].has_label = 1'b1;
        instr_list[i+branch_offset].is_branch_target = 1;
      end
    end
  endfunction

endclass

class riscv_sw_interrupt_instr extends riscv_directed_instr_stream;

  rand bit usip;
  rand bit ssip;
  rand bit msip;
  rand privileged_reg_t ip_reg;
  rand riscv_pseudo_instr li_instr;
  rand riscv_instr_base csr_instr;
  riscv_privil_reg ip;
  rand riscv_reg_t rs1_reg;

  constraint ip_reg_c {
    if(cfg.init_privileged_mode == MACHINE_MODE) {
      ip_reg == MIP;
    } else {
      ip_reg == SIP;
    }
    (ip_reg == MIP) -> (usip || ssip || msip);
    (ip_reg == SIP) -> (usip || ssip);
  }

  constraint instr_c {
    !(rs1_reg inside {cfg.reserved_regs});
    rs1_reg != ZERO;
    li_instr.pseudo_instr_name == LI;
    li_instr.rd == rs1_reg;
    csr_instr.instr_name == CSRRW;
    csr_instr.rs1 == rs1_reg;
    // TODO: Support non-zero rd for SIP, MIP
    // csr_instr.rd inside {cfg.avail_regs};
    csr_instr.rd == ZERO;
    csr_instr.csr == ip_reg;
  }

  `uvm_object_utils(riscv_sw_interrupt_instr)

  function new(string name = "");
    super.new(name);
    li_instr = riscv_pseudo_instr::type_id::create("li_instr");
    csr_instr = riscv_instr_base::type_id::create("csr_instr");
    ip = riscv_privil_reg::type_id::create("ip");
  endfunction

  function void post_randomize();
    // TODO: Support UIP
    if(cfg.init_privileged_mode == USER_MODE) return;
    ip.init_reg(ip_reg);
    if(ip_reg == SIP) begin
      ip.set_field("USIP", usip);
      ip.set_field("SSIP", ssip);
    end else begin
      ip.set_field("USIP", usip);
      ip.set_field("SSIP", ssip);
      ip.set_field("MSIP", msip);
    end
    li_instr.imm_str = $sformatf("0x%0x", ip.get_val());
    csr_instr.comment = ip_reg.name();
    instr_list = {li_instr, csr_instr};
    super.post_randomize();
  endfunction

endclass

`ifdef ENABLE_VECTORS

/* sjd ctrl_l 88
sew: 8, 16, 32
vlmul: 1, 2, 4, 8
vl can be 0-512 (only 512 when sew=8 and vlmul=8)
so buckets for vector integer instructions:
{sew, vlmul, vl} with v0.t and without
eg set vl to vlmax = -1
    li x4, -1
    vsetvli x3, x4, e16,m1
*/
class vsetvli_instr extends riscv_directed_instr_stream;

  riscv_instr_base          li;
  rand riscv_reg_t          li_rd;
  rand int                  li_imm;
//  rand int                  mixed_instr_cnt;

  riscv_instr_base          vsetvli;
  rand riscv_reg_t          vsetvli_rd;
  rand riscv_vtype_vsew_t   vsetvli_vsew;
  rand riscv_vtype_vmul_t   vsetvli_vmul;
  rand riscv_vtype_vediv_t  vsetvli_vediv;

  constraint instr_c {
    !(li_rd inside {cfg.reserved_regs, ZERO});
    li_imm inside {[0:15]}; // vl
    !(vsetvli_rd inside {cfg.reserved_regs, ZERO});
//    mixed_instr_cnt inside {[5:10]};
  }

  `uvm_object_utils(vsetvli_instr)

  function new(string name = "");
    super.new(name);
    li       = riscv_instr_base::type_id::create("li");
    vsetvli  = riscv_instr_base::type_id::create("vsetvli");
  endfunction


  function void post_randomize();
//    riscv_instr_base instr[];
    `DV_CHECK_RANDOMIZE_WITH_FATAL(li,
      instr_name    == LI;
      rd            == li_rd;
      imm           == li_imm;
    )
    `DV_CHECK_RANDOMIZE_WITH_FATAL(vsetvli,
      instr_name    == VSETVLI;
      rd            == vsetvli_rd;
      rs1           == li_rd;
      vtype_vsew    == vsetvli_vsew;
      vtype_vmul    == vsetvli_vmul;
      vtype_vediv   == vsetvli_vediv;
    )
    //initialize_instr_list(mixed_instr_cnt);
    //gen_instr(1'b1);
    //mix_instr_stream(instr);

    instr_list = {instr_list, li};
    instr_list = {instr_list, vsetvli};
  endfunction
endclass

class vadd_vi_instr_save extends riscv_directed_instr_stream;
  mem_region_t    data_page[$];
  string          data_name;
  int             data_size;
  int             offset;
  
  rand int        instr_cnt;
  
  riscv_pseudo_instr        i_la;
  
  riscv_instr_base          i_li;
  rand riscv_reg_t          r_vl;
  rand int                  v_vl;

  riscv_instr_base          vsetvli;
  rand riscv_reg_t          r_rd;
  rand riscv_vtype_vsew_t   vsetvli_vsew;
  rand riscv_vtype_vmul_t   vsetvli_vmul;

  rand riscv_reg_t          r_data_addr;
 
  riscv_instr_base          i_addi_0;
  riscv_instr_base          i_vle_v_0;
  riscv_instr_base          i_vle_v_1;
  
  rand riscv_vpr_t          vpr_0;
  rand riscv_vpr_t          vpr_1;
  
  riscv_instr_base          i_li_1;
  
  riscv_instr_base          i_vadd_vi;
  rand int                  v_imm;
  
  constraint instr_c {
    !(r_vl inside {ZERO});
    !(r_rd inside {ZERO});
    !(r_data_addr inside {ZERO});
    
    r_vl != r_data_addr;
    r_rd != r_data_addr;
    r_vl != r_rd;
    
    // TODO should really be v_vl inside {[0:512]};
    v_vl inside {0,5,8,13,16,20,32,47,64,83,115,128,156,211,256,333,416,501,512};
    
    vpr_0 != vpr_1;
    
    v_imm inside {[-16:16]};
  }

  `uvm_object_utils(vadd_vi_instr_save)

  function new(string name = "");
    super.new(name);
    i_li       = riscv_instr_base::type_id::create("li");
    i_la       = riscv_pseudo_instr::type_id::create("la");
    vsetvli    = riscv_instr_base::type_id::create("vsetvli");
    i_addi_0   = riscv_instr_base::type_id::create("addi");
    i_vle_v_0  = riscv_instr_base::type_id::create("vle_v");
    i_vle_v_1  = riscv_instr_base::type_id::create("vle_v");
    i_li_1     = riscv_instr_base::type_id::create("li");
    i_vadd_vi  = riscv_instr_base::type_id::create("vadd_vi");
  endfunction
  
  function void pre_randomize();
    data_page = cfg.mem_region;
    data_name = data_page[0].name;
    data_size = data_page[0].size_in_bytes;
    offset = 8; // TODO should this be derived or it is width of gpr in bytes
  endfunction
  riscv_instr_base     jump[];

  function void post_randomize();
    int                     test_num;
    riscv_instr_base        i_nop_start;
    riscv_instr_base        i_nop_end;
    
    i_nop_start      = riscv_instr_base::type_id::create("nop");
    i_nop_end        = riscv_instr_base::type_id::create("nop");
    `DV_CHECK_RANDOMIZE_WITH_FATAL(i_nop_start,
      instr_name    == NOP;
      rs1 == ZERO;
      rs2 == ZERO;
      rd  == ZERO;
    )
    `DV_CHECK_RANDOMIZE_WITH_FATAL(i_nop_end,
      instr_name    == NOP;
      rs1 == ZERO;
      rs2 == ZERO;
      rd  == ZERO;
    )
    i_nop_start.comment     = $sformatf(" start of vector setup, test: %0d", test_num);
    instr_list = {instr_list, i_nop_start};
    
    i_la.pseudo_instr_name  = LA;
    i_la.imm_str            = $sformatf("%0s+%0d", data_name, test_num*offset);
    i_la.rd                 = r_data_addr;
    i_la.comment            = " address for test data +offset";
    instr_list = {instr_list, i_la};
    
    `DV_CHECK_RANDOMIZE_WITH_FATAL(i_li,
      instr_name    == LI;
      rd            == r_vl;
      imm           == -1;
    )
    i_li.comment            = " Set VL to VLMAX to load values into registers";
    instr_list = {instr_list, i_li};

    `DV_CHECK_RANDOMIZE_WITH_FATAL(vsetvli,
      instr_name    == VSETVLI;
      rd            == r_rd;
      rs1           == r_vl;
      vtype_vsew    == vsetvli_vsew;
      vtype_vmul    == vsetvli_vmul;
      vtype_vediv   == D1; // TODO randomize when vediv spec sorted
    )
    instr_list = {instr_list, vsetvli};
    
    `DV_CHECK_RANDOMIZE_WITH_FATAL(i_addi_0,
      instr_name    == ADDI;
      rd            == r_data_addr;
      rs1           == r_data_addr;
      imm           == offset*8;
    )
    instr_list = {instr_list, i_addi_0};
    
    `DV_CHECK_RANDOMIZE_WITH_FATAL(i_vle_v_0,
      instr_name    == VLE_V;
      vd            == vpr_0;
      rs1           == r_data_addr;
      vm            != ZERO;
    )
    i_vle_v_0.comment            = " Load value into vs2";
    instr_list = {instr_list, i_vle_v_0};    
    
    instr_list = {instr_list, i_addi_0};
    
    `DV_CHECK_RANDOMIZE_WITH_FATAL(i_vle_v_1,
      instr_name    == VLE_V;
      vd            == vpr_1;
      rs1           == r_data_addr;
      vm            != ZERO;
    )
    i_vle_v_1.comment            = " Load value into vd";
    instr_list = {instr_list, i_vle_v_1};
    
    instr_list = {instr_list, i_addi_0};
    
    `DV_CHECK_RANDOMIZE_WITH_FATAL(i_li_1,
      instr_name    == LI;
      rd            == r_vl;
      imm           == v_vl;
    )
    i_li_1.comment            = $sformatf(" VL = %0d", v_vl);
    instr_list = {instr_list, i_li_1};

    instr_list = {instr_list, vsetvli};

    // the instruction being tested!
        `DV_CHECK_RANDOMIZE_WITH_FATAL(i_vadd_vi,
      instr_name    == VADD_VI;
      vd            == vpr_1;
      vs2           == vpr_0;
      imm           == v_imm;
      vm           != ZERO;
    )
    i_vadd_vi.comment = 
        $sformatf(" Test %0d: SEW = %0s, LMUL = %0s, Use Mask = 0, VL = %0d", 
                            test_num, vsetvli_vsew, vsetvli_vmul, v_vl);
    instr_list = {instr_list, i_vadd_vi};    
    
    i_nop_end.comment       = " end of vector test";
    instr_list = {instr_list, i_nop_end};
  endfunction
endclass

// this is first attempt at generating one instruction with legal vsetvli before
// TODO - need to add a loop - so this is a stream rather than one test
//      and need to not go past end of data
// TODO - vm
//      and need to not go past end of mask data
// TODO - how to stop others being put on top - how to make atomic...
// why do I keep getting: eg:
//  Info 5366: 'riscvOVPsim/cpu', 0x0000000080000d10(_main+bd6): Machine 0213b357 vadd.vi v6,v1,7
//  Warning (RISCV_IVI) CPU 'riscvOVPsim/cpu' 0x80000d10 0213b357 vadd.vi v6,v1,7: Illegal vector register index
// why is imm -16 to 16?
class vadd_vi_instr_save2 extends riscv_directed_instr_stream;
  mem_region_t    data_page[$];
  string          data_name;
  int             data_size;
  int             offset;
  rand int unsigned       num_tests;
  
  riscv_instr_base       vinstr[];
  riscv_pseudo_instr     pseudo[];
  
  rand riscv_reg_t          r_vl;
  rand riscv_reg_t          r_rd;
  rand riscv_reg_t          r_data_addr;
  rand riscv_vpr_t          vpr_0;
  rand riscv_vpr_t          vpr_1;
  rand riscv_vtype_vsew_t   vsetvli_vsew;
  rand riscv_vtype_vmul_t   vsetvli_vmul;
  rand int                  v_vl;
  rand int                  v_imm;
  
  constraint instr_c {
    !(r_vl inside {ZERO});
    !(r_rd inside {ZERO});
    !(r_data_addr inside {ZERO});
    
    r_vl != r_data_addr;
    r_rd != r_data_addr;
    r_vl != r_rd;
    
    // TODO should really be v_vl inside {[0:512]};
    v_vl inside {0,5,8,13,16,20,32,47,64,83,115,128,156,211,256,333,416,501,512};
    
    vpr_0 != vpr_1;
    
    v_imm inside {[-16:16]};
    
    num_tests inside {[1:2]};
   }
  `uvm_object_utils(vadd_vi_instr_save2)

  function new(string name = "");
    super.new(name);
  endfunction
  
  function void pre_randomize();
    data_page = cfg.mem_region;
    data_name = data_page[0].name;
    data_size = data_page[0].size_in_bytes;
    offset = 8; // TODO should this be derived or it is width of vpr in bytes
  endfunction

  function void post_randomize();
    int     test;
    int     num_vinstr_per_test;
    int     num_pseudo_per_test;
    int     ii;
    int     pp;
    
    num_vinstr_per_test   = 17;
    num_pseudo_per_test   = 17;
    ii  = 0;
    pp = 0;
    vinstr           = new[num_tests * num_vinstr_per_test];
    pseudo           = new[num_tests * num_pseudo_per_test];
    
    for (test=0; test<num_tests; test++) begin
        vinstr[ii] = riscv_instr_base::type_id::create("nop");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii], 
                             instr_name==NOP; rs1==ZERO; rs2==ZERO; rd==ZERO;)
        vinstr[ii].comment = $sformatf(" start of vector setup, test: %0d", test);
        instr_list = {instr_list, vinstr[ii++]};
        
        // la x2, test_1_data+0
        pseudo[pp]                    = riscv_pseudo_instr::type_id::create("la");
        pseudo[pp].pseudo_instr_name  = LA;
        pseudo[pp].imm_str            = $sformatf("%0s+%0d", data_name, test*offset);
        pseudo[pp].rd                 = r_data_addr;
        pseudo[pp].comment            = " address for test data +offset";
        instr_list = {instr_list, pseudo[pp++]};
        
        // li x4, -1
        vinstr[ii] = riscv_instr_base::type_id::create("li");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii],
            instr_name==LI;
            rd==r_vl;
            imm==-1;)
        vinstr[ii].comment = " Set VL to VLMAX to load values into registers";
        instr_list = {instr_list, vinstr[ii++]};
        
        // vsetvli x3, x4, e16,m1
        vinstr[ii] = riscv_instr_base::type_id::create("vsetvli");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii],
          instr_name    == VSETVLI;
          rd            == r_rd;
          rs1           == r_vl;
          vtype_vsew    == vsetvli_vsew;
          vtype_vmul    == vsetvli_vmul;
          vtype_vediv   == D1;) // TODO randomize when vediv spec sorted
        instr_list = {instr_list, vinstr[ii++]};
        
        // addi x2, x2, 64
        vinstr[ii] = riscv_instr_base::type_id::create("addi");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii],
          instr_name    == ADDI;
          rd            == r_data_addr;
          rs1           == r_data_addr;
          imm           == offset*8;)
        instr_list = {instr_list, vinstr[ii++]};

        // vle.v v15, (x2) 
        vinstr[ii] = riscv_instr_base::type_id::create("vle_v");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii],
          instr_name    == VLE_V;
          vd            == vpr_0;
          rs1           == r_data_addr;
          vm            != ZERO;)
        vinstr[ii].comment = " Load value into vs2";
        instr_list = {instr_list, vinstr[ii++]};    
        
        // addi x2, x2, 64
        vinstr[ii] = riscv_instr_base::type_id::create("addi");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii],
          instr_name    == ADDI;
          rd            == r_data_addr;
          rs1           == r_data_addr;
          imm           == offset*8;)
        instr_list = {instr_list, vinstr[ii++]};

        // vle.v v0, (x2)
        vinstr[ii] = riscv_instr_base::type_id::create("vle_v");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii],
          instr_name    == VLE_V;
          vd            == vpr_1;
          rs1           == r_data_addr;
          vm            != ZERO;)
        vinstr[ii].comment = " Load value into vd";
        instr_list = {instr_list, vinstr[ii++]};    
        
        // addi x2, x2, 64
        vinstr[ii] = riscv_instr_base::type_id::create("addi");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii],
          instr_name    == ADDI;
          rd            == r_data_addr;
          rs1           == r_data_addr;
          imm           == offset*8;)
        instr_list = {instr_list, vinstr[ii++]};

        // li  x4, 32
        vinstr[ii] = riscv_instr_base::type_id::create("li");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii],
            instr_name==LI;
            rd==r_vl;
            imm==v_vl;)
        vinstr[ii].comment = $sformatf(" VL = %0d", v_vl);
        instr_list = {instr_list, vinstr[ii++]};
        
        // vsetvli x3, x4, e16,m1
        vinstr[ii] = riscv_instr_base::type_id::create("vsetvli");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii],
          instr_name    == VSETVLI;
          rd            == r_rd;
          rs1           == r_vl;
          vtype_vsew    == vsetvli_vsew;
          vtype_vmul    == vsetvli_vmul;
          vtype_vediv   == D1;) // TODO randomize when vediv spec sorted
        instr_list = {instr_list, vinstr[ii++]};
        
        // the instruction being tested!
        vinstr[ii] = riscv_instr_base::type_id::create("vadd_vi");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii],
          instr_name    == VADD_VI;
          vd            == vpr_1;
          vs2           == vpr_0;
          imm           == v_imm;
          vm           != ZERO;)
        vinstr[ii].comment = 
            $sformatf(" Test %0d: SEW = %0s, LMUL = %0s, Use Mask = 0, VL = %0d", 
                                test, vsetvli_vsew, vsetvli_vmul, v_vl);
        instr_list = {instr_list, vinstr[ii++]};    

        // end
        vinstr[ii]         = riscv_instr_base::type_id::create("nop");
        `DV_CHECK_RANDOMIZE_WITH_FATAL(vinstr[ii], 
                             instr_name==NOP; rs1==ZERO; rs2==ZERO; rd==ZERO;)
        vinstr[ii].comment = " end of vector test";
        instr_list = {instr_list, vinstr[ii++]};
    end
    
  endfunction
endclass

// HERE ****************************************************************
class VIA_VI_FORMAT_f;
    randc riscv_reg_t          r_vl;
    randc riscv_reg_t          r_rd;
    randc riscv_reg_t          r_rs1;
    randc riscv_reg_t          r_data_addr;
    randc riscv_reg_t          r_mask_addr;
    rand  riscv_vpr_t          vpr_0;
    rand  riscv_vpr_t          vpr_1;
    rand  riscv_vpr_t          vpr_2;
    randc riscv_vtype_vsew_t   vsetvli_vsew;
    rand  riscv_vtype_vmul_t   vsetvli_vmul;
    randc int                  v_vl;
    randc int                  v_imm;
    rand  riscv_vins_vm_t      vm;
    
    constraint instr_c {
        !(r_vl inside {ZERO});
        !(r_rd inside {ZERO});
        !(r_data_addr inside {ZERO});
        !(r_mask_addr inside {ZERO});
    
        r_vl != r_data_addr;
        r_rd != r_data_addr;
        r_vl != r_rd;
    
        if (vm != V0_T) { // reversed?
            vpr_0 != V0; 
            vpr_1 != V0; 
            vpr_2 != V0;
        }

        v_vl inside {0,5,8,13,16,20,32,47,64,83,115,128,156,211,256,333,416,501,512};

        v_imm inside {[-16:15]};

        vsetvli_vmul dist { M1 := 1,
                            M2 := 4,
                            M4 := 8,
                            M8 := 16};
        if (vsetvli_vmul == M2) {
            vpr_0 inside {V0,V2,V4,V6,V8,V10,V12,V14,V16,V18,V20,V22,V24,V26,V28,V30};
            vpr_1 inside {V0,V2,V4,V6,V8,V10,V12,V14,V16,V18,V20,V22,V24,V26,V28,V30};
            vpr_2 inside {V0,V2,V4,V6,V8,V10,V12,V14,V16,V18,V20,V22,V24,V26,V28,V30};
        } else if (vsetvli_vmul == M4) {
            vpr_0 inside {V0,V4,V8,V12,V16,V20,V24,V28};
            vpr_1 inside {V0,V4,V8,V12,V16,V20,V24,V28};
            vpr_2 inside {V0,V4,V8,V12,V16,V20,V24,V28};
        } else if (vsetvli_vmul == M8) {
            vpr_0 inside {V0,V8,V16,V24};
            vpr_1 inside {V0,V8,V16,V24};
            vpr_2 inside {V0,V8,V16,V24};
        }
        solve vm before vpr_0;
        solve vm before vpr_1;
        solve vpr_0 before vsetvli_vmul;
        solve vpr_1 before vsetvli_vmul;
    }
endclass

class vadd_vi_instr extends riscv_directed_instr_stream;
  mem_region_t    data_page[$];
  string          data_name;
  string          mask_name;
  int             data_size;
  int             offset;
  int unsigned       num_tests = 1;
  
  riscv_instr_base       vinstr[];
  riscv_pseudo_instr     pseudo[];
  
  `uvm_object_utils(vadd_vi_instr)

  function new(string name = "");
    super.new(name);
  endfunction
  
  function void pre_randomize();
    data_page = cfg.mem_region;
    data_name = data_page[0].name;
    mask_name = data_page[0].name;
    data_size = data_page[0].size_in_bytes;
    offset = 8; // TODO should this be derived or it is width of vpr in bytes
  endfunction
  
  function riscv_instr_base gen_nop (string comment);
    riscv_instr_base instr = riscv_instr_base::type_id::create("nop");
    `DV_CHECK_RANDOMIZE_WITH_FATAL(instr, instr_name==NOP; rs1==ZERO; rs2==ZERO; rd==ZERO;)
    instr.comment = comment;
    instr.atomic  = 'b1;
    instr.has_label   = 'b0;
    return instr;
  endfunction
  function riscv_pseudo_instr gen_pseudo_rd_imm (string asm, riscv_pseudo_instr_name_t pseudo_instr_name, riscv_reg_t rd, string imm_str, string comment);
    riscv_pseudo_instr pseudo = riscv_pseudo_instr::type_id::create(asm);
    pseudo.pseudo_instr_name = pseudo_instr_name;
    pseudo.rd      = rd;
    pseudo.imm_str = imm_str;
    pseudo.comment = comment;
    pseudo.atomic  = 'b1;
    pseudo.has_label   = 'b0;
    return pseudo;
  endfunction
  function riscv_instr_base gen_instr_vsetvli (string asm, riscv_instr_name_t instr_name, riscv_reg_t rd, rs1, riscv_vtype_vsew_t vsew, riscv_vtype_vmul_t vmul, riscv_vtype_vediv_t vediv);
    riscv_instr_base instr = riscv_instr_base::type_id::create(asm);
    `DV_CHECK_RANDOMIZE_WITH_FATAL(instr,
      instr_name    == VSETVLI;
      vtype_vsew    == vsew;
      vtype_vmul    == vmul;
      vtype_vediv   == vediv;)
      instr.atomic  = 'b1;
      instr.has_label   = 'b0;
      instr.rd            = rd;
      instr.rs1           = rs1;
    return instr;
  endfunction
  function riscv_instr_base gen_instr_addi (string asm, riscv_instr_name_t instr_name, riscv_reg_t rd, rs1, string imm_str);
    riscv_instr_base instr = riscv_instr_base::type_id::create(asm);
    `DV_CHECK_RANDOMIZE_WITH_FATAL(instr,
      instr_name    == ADDI;)
      instr.atomic  = 'b1;
      instr.has_label   = 'b0;
      instr.rd            = rd;
      instr.rs1           = rs1;
      instr.imm_str           = imm_str;
    return instr;
  endfunction
  function riscv_instr_base gen_instr_vle_v (string asm, riscv_instr_name_t instr_name, riscv_vpr_t vd, riscv_reg_t rs1);
    riscv_instr_base instr = riscv_instr_base::type_id::create(asm);
    `DV_CHECK_RANDOMIZE_WITH_FATAL(instr,
      instr_name    == VLE_V;
      vm            != ZERO;)
      instr.atomic  = 'b1;
      instr.has_label   = 'b0;
      instr.vd            = vd;
      instr.rs1           = rs1;
    return instr;
  endfunction
  function riscv_instr_base gen_vadd_vi (string asm, riscv_instr_name_t instr_name, riscv_vpr_t vd, vs2, string imm_str, riscv_vins_vm_t vm);
    riscv_instr_base instr = riscv_instr_base::type_id::create(asm);
    `DV_CHECK_RANDOMIZE_WITH_FATAL(instr,
      instr_name    == VADD_VI;)
      instr.atomic  = 'b1;
      instr.has_label   = 'b0;
      instr.vd            = vd;
      instr.vs2           = vs2;
      instr.vm           = vm;
      instr.imm_str           = imm_str;
    return instr;
  endfunction

  function void post_randomize();
    int     test;
    const int     NUM_VINSTR_PER_TEST = 17;
    const int     NUM_PSEUDO_PER_TEST = 17;
    int     ii = 0;
    int     pp = 0;
    VIA_VI_FORMAT_f ins;
    ins = new;
    `DV_CHECK_RANDOMIZE_FATAL(ins);
    
    vinstr           = new[num_tests * NUM_VINSTR_PER_TEST];
    pseudo           = new[num_tests * NUM_PSEUDO_PER_TEST];
    
    vinstr[ii] = gen_nop ($sformatf(" start of vector setup, test: %0d", test));
    instr_list = {instr_list, vinstr[ii++]};
    if (ins.vm != V0_T) begin // reversed?
        vinstr[ii] = gen_nop ($sformatf("   vm setup"));
        instr_list = {instr_list, vinstr[ii++]};
        pseudo[pp] = gen_pseudo_rd_imm ("li", LI, ins.r_vl, "-1", " Set VL to VLMAX to load values into registers");
        instr_list = {instr_list, pseudo[pp++]};
        vinstr[ii] = gen_instr_vsetvli ("vsetvli", VSETVLI, ins.r_rd, ins.r_vl, ins.vsetvli_vsew, ins.vsetvli_vmul, D1);
        instr_list = {instr_list, vinstr[ii++]};
        pseudo[pp] = gen_pseudo_rd_imm ("la", LA, ins.r_mask_addr, $sformatf("%0s", mask_name), "");
        instr_list = {instr_list, pseudo[pp++]};
        vinstr[ii] = gen_instr_vle_v ("vle_v", VLE_V, V0, ins.r_mask_addr);
        instr_list = {instr_list, vinstr[ii++]};
    end
    pseudo[pp] = gen_pseudo_rd_imm ("la", LA, ins.r_data_addr, $sformatf("%0s+%0d", data_name, test*offset), " address for test data +offset");
    instr_list = {instr_list, pseudo[pp++]};
    pseudo[pp] = gen_pseudo_rd_imm ("li", LI, ins.r_vl, "-1", " Set VL to VLMAX to load values into registers");
    instr_list = {instr_list, pseudo[pp++]};
    vinstr[ii] = gen_instr_vsetvli ("vsetvli", VSETVLI, ins.r_rd, ins.r_vl, ins.vsetvli_vsew, ins.vsetvli_vmul, D1);  // TODO randomize when vediv spec sorted
    instr_list = {instr_list, vinstr[ii++]};
    vinstr[ii] = gen_instr_addi ("addi", ADDI, ins.r_data_addr, ins.r_data_addr,"0");
    instr_list = {instr_list, vinstr[ii++]};
    vinstr[ii] = gen_instr_vle_v ("vle_v", VLE_V, ins.vpr_0, ins.r_data_addr);
    instr_list = {instr_list, vinstr[ii++]};
    vinstr[ii] = gen_instr_addi ("addi", ADDI, ins.r_data_addr, ins.r_data_addr,"4");
    instr_list = {instr_list, vinstr[ii++]};
    vinstr[ii] = gen_instr_vle_v ("vle_v", VLE_V, ins.vpr_1, ins.r_data_addr);
    instr_list = {instr_list, vinstr[ii++]};
//    vinstr[ii] = gen_instr_addi ("addi", ADDI, ins.r_data_addr, ins.r_data_addr,"8"); // if 3 operands needed
//    instr_list = {instr_list, vinstr[ii++]};
//    vinstr[ii] = gen_instr_vle_v ("vle_v", VLE_V, ins.vpr_2, ins.r_data_addr);
//    instr_list = {instr_list, vinstr[ii++]};
    pseudo[pp] = gen_pseudo_rd_imm ("li", LI, ins.r_vl, $sformatf("%0d", ins.v_vl), " Set VL");
    instr_list = {instr_list, pseudo[pp++]};
    vinstr[ii] = gen_instr_vsetvli ("vsetvli", VSETVLI, ins.r_rd, ins.r_vl, ins.vsetvli_vsew, ins.vsetvli_vmul, D1);
    instr_list = {instr_list, vinstr[ii++]};
    
    vinstr[ii] = gen_vadd_vi ("vadd_vi", VADD_VI, ins.vpr_1, ins.vpr_0,  $sformatf("%0d", ins.v_imm), ins.vm);
    instr_list = {instr_list, vinstr[ii++]};

    vinstr[ii] = gen_nop ($sformatf(" end, test: %0d", test));
    instr_list = {instr_list, vinstr[ii++]};

  endfunction
endclass
`endif

