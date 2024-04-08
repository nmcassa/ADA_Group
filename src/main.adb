with Ada.Text_IO;
use Ada.Text_IO;

procedure MAIN is 
	-- create an index for our board array from (0 - 8)
	type Index is range 0 .. 8;
	type Board is
      	 array (Index) of Character;

	B : Board := ('b', 'b', 'b', 'a', 'a'
			, 'a', 'l', 'l', 'l');

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

begin
	Temp1 := DisplayBoard(B);
end MAIN;
