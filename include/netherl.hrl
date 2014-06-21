%
%
-record(program, { stmts=[] }).

%
%
-record(execution, 
        {
            direction=north, 
            location={0,0}, 
            instr_index = 0
        }).

%
%
-record(world, { blocks }).

%
%
-record(block, {}).