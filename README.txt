README for EMECHS Scheme
--------------------------

EMECHS is a simple interpreter for a sub-set of the Scheme programming language. It is designed as a demonstration of both the process of writing an interpreter, and as an example of a large program written in hand-coded MIPS assembly language. Its primary purpose is as a tutorial for assembly language students on basic techniques of assembly language programming.

The primary file for the program is emechs.s, an assembly language source file which can be assembled and run using the SPIM simulator. 

The current version of EMECHS Scheme can be run using the SPIM simulator, preferably the most recent version (QtSpim), which can be gotten from the Sourceforge page for the project (http://sourceforge.net/projects/spimsimulator/). In order to run the simulator from QtSpim, launch the QtSpim application. You must then go to Simulator->Settings and set both 'Enalbe Delayed Branches' and 'Enable Delayed Loads' - these settings are necessary for the program to work correctly. Once this is done, you can load the program choosing File->Load File and searching for the emechs.s file. After loading the program, it can be run by choosing Simulator->Run/Continue from the main menu.