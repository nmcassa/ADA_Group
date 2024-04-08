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

	Temp1 : Integer;

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
	begin
		-- Error Check if I is outside of 0 .. 8
		B(I) := Player;
		return 1;
	end PlayMove;

begin
	--Inside of here we need to make game loop
	--TODO
	Temp1 := DisplayBoard(B);

	Temp1 := PlayMove(B, 4, 'X');

	Ada.Text_IO.New_Line;
	Ada.Text_IO.Put("After Change");
	Ada.Text_IO.New_Line;

	Temp1 := DisplayBoard(B);
end MAIN;
