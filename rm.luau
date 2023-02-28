--[[
Copyright (c) 2023 Billy Snikkers billys@hey.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
--]]

-- There are two kinds of commands
-- "Ri+=>n" -- increment register i, then go to command n
-- "Ri-=>n,m" -- if register i is non-zero, decrement and goto command n, else go to command m
export type Command = {
	i: number,
	op: "+",
	goto: number
} | {
	i: number,
	op: "-",
	goto: number,
	orto: number,
}

export type Program = {Command}
export type Registers = {number}

local function inc(Ri: number, goto: number): Command
	return {
		i = Ri,
		op = "+",
		goto = goto,
	}
end

local function dec(Ri: number, goto: number, orto: number): Command
	return {
		i = Ri,
		op = "-",
		goto = goto,
		orto = orto,
	}
end

local function parseCommand(str: string): Command
  local num1, op, num2, num3 = string.match(str, "R(%d+)([+-])=>(%d+),?(%d*)")
	num1 = tonumber(num1) :: number?
	num2 = tonumber(num2) :: number?
	num3 = tonumber(num3) :: number?
  if op == "+" and num1 and num2 and (not num3) then
		return {
			i = num1,
			op = op,
			goto = num2,
		}
	elseif op == "-" and num1 and num2 and num3 then
		return {
			i = num1,
			op = op,
			goto = num2,
			orto = num3,
		}
  end
end

local function parseProgram(commandStrings: {string}): Program
	local program: Program = {}
	for _, str in commandStrings do
		local command = parseCommand(str)
		if not command then
			error("Bad command parse: "..tostring(str))
		end
		table.insert(program, command)
	end
	return program
end

local function registerString(registers: Registers)
	local last = 1
	for i=#registers,1,-1 do
		if registers[i] ~= 0 then
			last = i
			break
		end
	end

	if #registers < 55 or last == #registers then
		return table.concat(registers, ",")
	end

	return table.concat(registers, ",", 1, last).."..[0]*"..(#registers - last)
end

local function commandString(command: Command)
	if command.op == "+" then
		return "R"..command.i.."+=>"..command.goto
	else
		assert(command.op == "-", "Bad command")
		return "R"..command.i.."-=>"..command.goto..","..command.orto
	end
end
	
local RM = {}

function RM.new(program: Program, registers: {number}?)
	local self = {
		registers = registers or {},
		program = program,
	}

	function self.maxRegister()
		local max = 1
		for _, command in self.program do
			max = math.max(max, command.i)
		end
		return max
	end

	function self.wipe()
		table.clear(self.registers)
		for i=1, self.maxRegister() do
			self.registers[i] = 0
		end
	end
	
	function self.validate()
		for i=1, math.max(#self.registers, self.maxRegister()) do
			local ri = self.registers[i]
			if type(ri) ~= "number" or ri < 0 or math.round(ri) ~= ri then
				error("Bad register".."R"..i.."= "..tostring(ri))
			end
		end
	end
	
	function self.run(showComputation: boolean?)
		self.validate()
		local i = 1
		while true do
			local command = self.program[i]
			if not command then
				return
			end
			if showComputation then
				local left = i..": "..commandString(command)
				local right = registerString(self.registers)
				print(left..string.rep(" ", math.max(1, 25-#left))..right)
			end
			if command.op == "+" then
				self.registers[command.i] += 1
				i = command.goto
			else
				assert(command.op == "-", "Bad command")
				if self.registers[command.i] > 0 then
					self.registers[command.i] -= 1
					i = command.goto
				else
					i = command.orto
				end
			end
		end
	end

	function self.compute(input: {number}, showComputation: boolean?)
		self.wipe()
		for i, v in input do
			self.registers[i] = v
		end
		self.run(showComputation)
		local result = self.registers[1]
		return result
	end

	return self
end

function RM.parseNew(commandStrings: {string})
	return RM.new(parseProgram(commandStrings))
end

local PR = {}

PR.zero = parseProgram {
	[1] = "R1-=>1,2"
}

PR.succ = parseProgram {
	[1] = "R1+=>2"
}

function PR.proj(i: number)
	if i==1 then
		return parseProgram {}
	else
		return parseProgram {
			[1] = "R1-=>1,2",
			[2] = "R"..i.."-=>3,4",
			[3] = "R1+=>2",
		}
	end
end

function PR.composePrograms(p: Program, q: Program) : Program

	local length_p = #p
	
	local result = {}

	for _, command in p do
		local newCommand = table.clone(command)
		if newCommand.goto > length_p then
			newCommand.goto = length_p+1
		end
		if newCommand.op == "-" then
			if newCommand.orto > length_p then
				newCommand.orto = length_p+1
			end
		end
		table.insert(result, newCommand)
	end

	for _, command in q do
		local newCommand = table.clone(command)
		newCommand.goto += length_p
		if newCommand.op == "-" then
			newCommand.orto += length_p
		end
		table.insert(result, newCommand)
	end

	return result
			
end

function PR.composeMorePrograms(ps: {Program}) : Program

	result = {}
	for _, p in ps do
		result = PR.composePrograms(result, p)
	end
	return result
end

function PR.compose(h: Program, ...: Program): Program
	---------------
	-- PLAN
	---------------
	-- [h registers][g1 registers]...[gn registers]
	-- Copy xi's from start to each gi zone
	-- Compute each gi
	-- Move results into h zone
	-- Run h

	local gs = {...}
	assert(#gs >= 1, "Cannot compose with zero programs")

	local gMax = 1
	for _, g in gs do
		for _, command in g do
			gMax = math.max(gMax, command.i)
		end
	end

	local hMax = math.max(#gs, gMax)
	for _, command in h do
		hMax = math.max(hMax, command.i)
	end

	local shiftedGs = {}
	local gStart = {}
	for k, g in gs do
		local start = hMax + 1 + (k-1)*gMax
		table.insert(gStart, start)

		-- Shift all register index updates in the program g
		local shiftedProgram = {}
		for _, command in g do
			local shiftedCommand = table.clone(command)
			shiftedCommand.i += start-1
			table.insert(shiftedProgram, shiftedCommand)
		end
		
		table.insert(shiftedGs, shiftedProgram)
	end

	local program = {}

	-- Ignore all input arguments after gMax
	-- We set them to zero so they don't interfere
	for j=hMax+1, hMax + gMax * #gs do
		local ni = #program+1
		program[ni] = dec(j, ni, ni+1)
	end

	-- Copy input from 1 -> gMax to all g register zones
	for j=1, gMax do
		local ni = #program+1
		-- Pull jth input from j
		program[ni] = dec(j, ni+1, ni+#gs+1)
		-- Push jth input to all g registers in the jth position
		for k=1, #gs-1 do
			program[ni + k] = inc(gStart[k] + (j-1), (ni + k + 1))
		end
		-- The last push, so loop back to the pull step
		program[ni + #gs] = inc(gStart[#gs] + (j-1), ni)
	end

	-- Modify gs to run in order
	-- This seems sound even if some of the gs are empty programs
	for _, shiftedG in shiftedGs do
		local lastEnd = #program
		local startOfNext = lastEnd + #shiftedG + 1
		for _, gCommand in shiftedG do
			local command = table.clone(gCommand)
			if command.goto > #shiftedG then
				command.goto = startOfNext
			else
				command.goto = lastEnd + command.goto
			end
			if command.op == "-" then
				if command.orto > #shiftedG then
					command.orto = startOfNext
				else
					command.orto = lastEnd + command.orto
				end
			end
			table.insert(program, command)
		end
	end

	-- Move the result of g_k into Rk
	for k=1, #shiftedGs do
		local start = gStart[k]
		local ni = #program+1
		program[ni] = dec(start, ni+1, ni+2)
		program[ni+1] = inc(k, ni)
	end

	local hStart = #program+1
	for _, hCommand in h do
		local command = table.clone(hCommand)
		if command.goto > #h then
			command.goto = hStart + #h
		else
			command.goto = hStart-1 + command.goto
		end
		if command.op == "-" then
			if command.orto > #h then
				command.orto = hStart + #h
			else
				command.orto = hStart-1 + command.orto
			end
		end
		table.insert(program, command)
	end

	return program
end



function PR.primrec(g: Program, h: Program): Program
	---------------
	-- PLAN
	---------------
	-- yx...
	-- 0x...[...]y
	-- 0x...[x...]y
	--       G
	-- 0x...[g...]y --> if y==0 then move Rc2 to R1 and finish
	-- 0x...[g....](y-1)
	-- 0x...[0g....](y-1)
	-- 0x...[0gx...](y-1)
	-- 1x...[0gx...](y-1)
	--       H
	-- 1x...[H(0,g,x)...](y-1) --> if (y-1)==0 then move Rc1 to R1 and finish
	-- 1x...[H(0,g,x)...](y-2)
	-- 1x...[0H(0,g,x)...](y-2)
	-- 1x...[1H(0,g,x)x...](y-2)
	-- 2x...[1H(0,g,x)x...](y-2)
	--       H
	-- 2x...[H(1,h0,x)...](y-2) --> if (y-2)==0 then move Rc1 to R1 and finish
	-- 2x...[H(1,h0,x)...](y-3)
	-- 2x...[0H(1,h0,x)...](y-3)
	-- 2x...[2H(1,h0,x)x...](y-3)

	local gMax = 0
	for _, command in g do
		gMax = math.max(gMax, command.i)
	end
	
	local hMax = 0 
	for _, command in h do
		hMax = math.max(hMax, command.i)
	end

	local M = math.max(gMax, hMax)

	local yUp = 1
	local xStart = 2
	local xEnd = M
	local compStart = xEnd + 1
	local compEnd = compStart + (M+1) - 1
	local yDown = compEnd + 1
	local mem = yDown + 1

	local program: Program = {}

	-- Clear memory register
	program[#program+1] = dec(mem, #program+1, #program+2)
	-- Clear yDown
	program[#program+1] = dec(yDown, #program+1, #program+2)
	
	do -- Move yUp to yDown
		local ni = #program + 1
		program[ni] = dec(yUp, ni+1, ni+2)
		program[ni+1] = inc(yDown, ni)
	end


	-- Copy xs to compute area
	for i=xStart, xEnd do
		local ni = #program+1
		local ix = i - xStart + 1

		-- Clear target for xi
		program[ni] = dec(compStart + ix - 1, ni, ni+1)
		-- Pull from i and push to compute area & memory
		program[ni+1] = dec(i, ni+2, ni+4)
		program[ni+2] = inc(compStart + ix - 1, ni+3)
		program[ni+3] = inc(mem, ni+1)
		-- Move memory back to i
		program[ni+4] = dec(mem, ni+5, ni+6)
		program[ni+5] = inc(i, ni+4)
	end

	-- Perform g on xs in compute area
	-- Will leave [v...] in the compute area, where v is the result of g
	-- This part is skipped if #g == 0, which works as expected
	do
		local gStart = #program + 1
		for _, gCommand in g do
			local command = table.clone(gCommand)
			-- Shift register indices to compute area
			command.i = compStart + command.i - 1

			-- Shift command goto/orto indices
			if command.goto > #g then
				command.goto = gStart + #g
			else
				command.goto = gStart + command.goto-1
			end
			if command.op == "-" then
				if command.orto > #g then
					command.orto = gStart + #g
				else
					command.orto = gStart + command.orto-1
				end
			end

			table.insert(program, command)
		end
	end
	

	-- Big loop time
	do
		local loop = #program + 1

		-- Check recursion register
		program[loop] = dec(yDown, loop+4, loop+1)

		do -- If recursion register empty...
			
			-- Clear R1
			program[loop+1] = dec(1, loop+1, loop+2)
			-- Pull from compStart or End
			program[loop+2] = dec(compStart, loop+3, 0) -- ORTO WILL BE UPDATED TO POSITIVE OUT OF BOUNDS
			-- Push to R1
			program[loop+3] = inc(1, loop+2)
		end

		-- Move result from compStart to compStart+1
		do
			-- Clear compStart+1
			program[loop+4] = dec(compStart+1, loop+4, loop+5)
			-- Pull from compStart
			program[loop+5] = dec(compStart, loop+6, loop+7)
			-- Push to compStart+1
			program[loop+6] = inc(compStart + 1, loop+5)
		end

		-- Copy yUp to compStart (will be empty)
		do
			-- Pull from yUp
			program[loop+7] = dec(yUp, loop+8, loop+10)
			-- Push to compStart and memory
			program[loop+8] = inc(compStart, loop+9)
			program[loop+9] = inc(mem, loop+7)
			-- Move memory back to yUp
			program[loop+10] = dec(mem, loop+11, loop+12)
			program[loop+11] = inc(yUp, loop+10)
		end

		-- Copy xs to compStart + 2 onward
		for i=xStart, xEnd do
			local ni = #program+1
			-- x-index of register i
			local ix = i - xStart + 1

			-- Clear target for xi
			program[ni] = dec((compStart + 2) + ix - 1, ni, ni+1)

			-- Pull from i and push to compute area & memory
			program[ni+1] = dec(i, ni+2, ni+4)
			program[ni+2] = inc((compStart + 2) + ix - 1, ni+3)
			program[ni+3] = inc(mem, ni+1)
			-- Move memory back to i
			program[ni+4] = dec(mem, ni+5, ni+6)
			program[ni+5] = inc(i, ni+4)
		end


		-- Increment yUp and loop if h is empty, otherwise do h on compute area
		if #h == 0 then
			program[#program+1] = inc(yUp, loop)
		else
			program[#program+1] = inc(yUp, #program+2)
		end

		-- Run H on compute area, and return to loop when done
		local hStart = #program + 1
		for _, hCommand in h do
			local command = table.clone(hCommand)
			-- Shift register indices to compute area
			command.i = compStart + command.i - 1

			-- Shift command goto/orto indices
			if command.goto > #h then
				-- Return to loop start
				command.goto = loop
			else
				command.goto = hStart + command.goto-1
			end
			if command.op == "-" then
				if command.orto > #h then
					-- Return to loop start
					command.orto = loop
				else
					command.orto = hStart + command.orto-1
				end
			end

			table.insert(program, command)
		end
	end


	-- Fix non-positive orto cases
	for _, command in program do
		if command.orto and command.orto <= 0 then
			command.orto = #program+1
		end
	end


	return program
end

function PR.min(h: Program): Program

	local program: Program = {}
	
	local hMax = 0
	for _, command in h do
		hMax = math.max(hMax, command.i)
	end

	local xStart = 1
	local xEnd = hMax-1 -- last position at hMax leaves room for prefixing with y in compute area
	local yReg = hMax + 1
	local compStart = yReg + 1
	local compEnd = compStart + hMax - 1
	local mem = compEnd + 1

	-- Clear memory register
	program[#program+1] = dec(mem, #program+1, #program+2)
	-- Clear yReg
	program[#program+1] = dec(yReg, #program+1, #program+2)

	-- Loop
	do
		local loop = #program + 1

		-- Write yReg to compstart
		do
			-- Clear compstart
			program[loop] = dec(compStart, loop, loop+1)
			-- Pull from yReg and push to compStart and memory
			program[loop+1] = dec(yReg, loop+2, loop+4)
			program[loop+2] = inc(compStart, loop+3)
			program[loop+3] = inc(mem, loop+1)
			-- Move memory back to yReg
			program[loop+4] = dec(mem, loop+5, loop+6)
			program[loop+5] = inc(yReg, loop+4)
		end


		-- Write xi's to compstart + 1 onward
		for i=xStart, xEnd do
			local ni = #program+1
			-- x-index of register i (this is just i, since xStart=1 in this case)
			local ix = i - xStart + 1

			-- Clear target for xi
			program[ni] = dec((compStart + 1) + ix - 1, ni, ni+1)

			-- Pull from i and push to compute area & memory
			program[ni+1] = dec(i, ni+2, ni+4)
			program[ni+2] = inc((compStart + 1) + ix - 1, ni+3)
			program[ni+3] = inc(mem, ni+1)
			-- Move memory back to i
			program[ni+4] = dec(mem, ni+5, ni+6)
			program[ni+5] = inc(i, ni+4)
		end

		-- Run H on compute area
		-- This works even if h is empty
		local hStart = #program + 1
		local hAfter = hStart + #h
		for _, hCommand in h do
			local command = table.clone(hCommand)
			-- Shift register indices to compute area
			command.i = compStart + command.i - 1

			-- Shift command goto/orto indices
			if command.goto > #h then
				command.goto = hAfter
			else
				command.goto = hStart + command.goto-1
			end
			if command.op == "-" then
				if command.orto > #h then
					command.orto = hAfter
				else
					command.orto = hStart + command.orto-1
				end
			end

			table.insert(program, command)
		end

		-- Inspect result for root
		program[hAfter] = dec(compStart, hAfter+1, hAfter+2)

		-- No root, increment yReg and restart loop
		program[hAfter+1] = inc(yReg, loop)

		-- Root found
		-- Move yReg to R1 and halt
		do
			-- Clear R1
			program[hAfter+2] = dec(1, hAfter+2, hAfter+3)
			-- Pull from yReg or halt using hAfter+6
			program[hAfter+3] = dec(yReg, hAfter+4, hAfter+6)
			-- Push to R1
			program[hAfter+4] = inc(1, hAfter+3)
		end
	end

	return program
end

-- selene: allow(unused_variable)
do
	local multiply = parseProgram {
		-- Move R1 to R3
		[1] = "R1-=>2,3",
		[2] = "R3+=>1",
	
		-- if y>0, need to add x to R1, otherwise done
		[3] = "R2-=>4,9",
	
		-- Add R3 to R1 and R4, counting R3 down to 0
		[4] = "R3-=>5,7",
		[5] = "R4+=>6",
		[6] = "R1+=>4",
	
		-- Move R4 back to R3, then go back and check R2
		[7] = "R4-=>8,3",
		[8] = "R3+=>7",
	}
	
	local o = PR.compose
	local rec = PR.primrec
	local min = PR.min
	local s = PR.succ
	local z = PR.zero
	local comp = PR.composePrograms
	local p1 = PR.proj(1)
	local p2 = PR.proj(2)
	local p3 = PR.proj(3)

	local zss = comp(comp(z,s),s)
	local z2 = comp({},s)
	local zsss = PR.composeMorePrograms({z,s,s,s})
	
	-- Some fun programs
	local factorial = rec(o(s,z), o(multiply, o(s, p1), p2))
	local pred = rec(z, p1)
	local subflip = rec(p1, o(pred, p2))
	local sub = o(subflip, p2, p1)
	local div = min(o(sub, p2, o(multiply, p1, p3)))

	local add = rec(p1, o(s, p2))
	local mul = rec(z, o(add, p2, p3))
	local pow = o(rec(o(s,z), o(mul, p2, p3)), p2, p1)

	local function con(n: number)
		assert(n >= 0, "n must be non-negative")
		if n == 0 then
			return z
		else
			return o(s, con(n-1))
		end
	end

	local function test(name: string, program: Program, input: {number}, showComputation: boolean?)
		local callStr = name.."("..table.concat(input, ",")..")"
		print(callStr.." = "..RM.new(program).compute(input, showComputation))
	end

	-- The computation for multiply is also printed out
	test("multiply", multiply, {3,5}, true) --> "multiply(3,5) = 15"
	test("factorial", factorial, {7}) 			--> "factorial(7) = 5040"
	test("pred", pred, {3}) 								--> "pred(3) = 2"
	test("sub", sub, {19,4}) 								--> "sub(19,4) = 15"
	test("div", div, {24,3}) 								--> "div(24,3) = 8"
	test("con(5)", con(5), {10}) 						--> "con(5)(10) = 5"
	test("add", add, {3,4})									--> "add(3,4) = 7"
	test("mul", mul, {3,4})									--> "mul(3,4) = 12"
	test("pow", pow, {3,4})									--> "pow(3,4) = 81"
	test("compose programs", z2, {2})						--> "s*s*z = 2"
	test("compose programs", zss, {})						--> "s*s*z = 2"
	test("compose programs", zsss, {})						--> "s*s*z = 2"
end
