FPGA
====

## URLs

	## https://doc.embedfire.com/fpga/altera/ep4ce10_mini/zh/latest/fpga/Preface.html
	## https://www.youtube.com/watch?v=nblGw37Fv8A&ab_channel=RenzymEducation
	## https://blog.csdn.net/zgcjaxj/article/details/104853081
	## https://github.com/steveicarus/iverilog
	## https://steveicarus.github.io/iverilog/usage/getting_started.html

## Tools

	## Online simulators
	https://edaplayground.com/

	## Icarus Verilog
	https://bleyer.org/icarus/

## Icarus Verilog on Ubuntu

	
	## 1 Installation
	$ sudo apt install Verilog
	$ sudo apt install gtkwave
	## 2 Write code for Verilog and testbench
	## 3 Compile and Simulate
	$ iverilog -dsn icarus_example.v
	$ vvp dsn
	## 4 show vcd in gtkwave
	$ gtkwave test.vcd

