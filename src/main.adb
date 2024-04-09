with Ada.Text_IO;
use Ada.Text_IO;

procedure MAIN is
	-- create an index for our board array from (0 - 8)
	type Index is range 0 .. 8;
	type Board is
      	 array (Index) of Character;

	B : Board := ('-', '-', '-',
			 '-', '-', '-',
			 '-', '-', '-');
	type IntroBoard is
	 array (Index) of Integer;

	Intro : IntroBoard := (0, 1, 2, 3, 4, 5, 6, 7, 8);
 

	Temp1 : Integer;

	Winner : Character;

	function IntroPrint(Intro : IntroBoard) return Integer
	is 
	begin
		Ada.Text_IO.Put("When prompted, select the space you want to make your move.");
		Ada.Text_IO.New_Line;
		for I in Index loop
			if I = 3 or I = 6 then
				Ada.Text_IO.New_Line; 
			end if;	
			Ada.Text_IO.Put(Integer'Image(Intro(I)));
		end loop;
		Ada.Text_IO.New_Line;
		return 1;

	end IntroPrint;

	function DisplayBoard(B : Board) return Integer 
	is
	begin
		for I in Index loop
			if I = 3 or I = 6 then
				Ada.Text_IO.New_Line; 
			end if;	
			Ada.Text_IO.Put(B(I));
		end loop;
		return 1;
	end DisplayBoard;

	function PlayMove(B : out Board; I : Index; Player : Character) return Integer
	is
		Temp : Integer;
		New_Move : Index;
	begin
		-- Error Check if I is outside of 0 .. 8
		if B(I) = '-' then
			B(I) := Player;
			return 1;
		end if;
		Ada.Text_IO.New_Line;
		Ada.Text_IO.Put("That space is already taken, select a new space: ");
		New_Move := Index'Value(Ada.Text_IO.Get_Line);
		Ada.Text_IO.New_Line;
		Temp := PlayMove(B, New_Move, Player);
		return 1;
	end PlayMove;

	function GameLoop (B : out Board) return Character
	is
		looping : Integer;
		Move : Index;
		Temp : Integer;
	begin
		looping := 1;

		Ada.Text_IO.New_Line;
		Temp := DisplayBoard(B);
		Ada.Text_IO.New_Line;
	
		while looping = 1 loop
			-- PlayerX's move
			Ada.Text_IO.New_Line;
			Ada.Text_IO.Put("Player X Make a Move: ");

			Move := Index'Value(Ada.Text_IO.Get_Line);			
			Temp := PlayMove(B, Move, 'X');

			Ada.Text_IO.New_Line;
			Temp := DisplayBoard(B);
			Ada.Text_IO.New_Line;

			-- Check if X won
			-- if X won, return X

			-- PlayerO's move
			Ada.Text_IO.New_Line;
			Ada.Text_IO.Put("Player O Make a Move: ");

			Move := Index'Value(Ada.Text_IO.Get_Line);			
			Temp := PlayMove(B, Move, 'O');

			Ada.Text_IO.New_Line;
			Temp := DisplayBoard(B);
			Ada.Text_IO.New_Line;

			-- Check if O won

		end loop;
		return 'X';

	end GameLoop;

begin
	Temp1 := IntroPrint(Intro);
	Winner := GameLoop(B);	
end MAIN;
