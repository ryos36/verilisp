
assign x = 4'b0000;
module sim_count(clk, debug_v);
    input clk;
    input debug_v;
    logic [15 : 0] count;
    logic debug_v_d;
    assign debug_v_d = 4'h14D;
    always_ff
        @(posedge clk)
    begin
        if ((debug_v <= (16'h1234 + 16'h1234)))
        begin
            debug_v_d <= #1 debug_v;
            debug_v_d <= debug_v;
        end
    end
endmodule
