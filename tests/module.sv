
module test(a, b, c, d, e);
    output a;
    inout [1 : 0] b;
    inout [1 : 0] c;
    input d;
    input e;
    $display(b, c, d, e);
endmodule

module test2();
    logic a;
    logic b0;
    logic b1;
    logic d_;
    logic c0;
    logic c1;
    logic e;
    test anon_0 (a, {b0, b1}, .d(d_), .c({c0, c1}), e);
    $display(2);
endmodule

module clock(clk);
    output clk;
    always_comb
    begin
        clk = (!clk);
    end
endmodule

module test(clk);
    input clk;
    logic [2 : 0] HD_valid_r;
    logic HD_valid;
    always_ff
        @(posedge clk)
    begin
        HD_valid_r <= {HD_valid_r[1 : 0], HD_valid};
    end
endmodule
