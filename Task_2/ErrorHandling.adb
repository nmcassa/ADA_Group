with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure TicTacToe is


   -- Exception for invalid input
   Invalid_Input : exception;  
   
   
   
   
   
   -- Subprogram for checking if a move is valid
   function Valid_Move (Board : in Board_Type; Row, Col : in Integer) return Boolean is
   begin
      -- Check if the move is within the bounds of the board
      return Row >= 1 and then Row <= 3 and then Col >= 1 and then Col <= 3 and then
             Board(Row, Col) = ' '; -- Check if the cell is empty
   end Valid_Move;





 -- Subprogram for getting user input
   procedure Get_User_Input (Row, Col : out Integer) is
   begin
      Put_Line("Enter row (1-3): ");
      Get(Row);

      Put_Line("Enter column (1-3): ");
      Get(Col);

      -- Validate user input
      if Row < 1 or else Row > 3 or else Col < 1 or else Col > 3 then
         raise Invalid_Input;
      end if;
   exception
      when Data_Error =>
         raise Invalid_Input;
   end Get_User_Input;








   -- Subprogram for handling errors and displaying messages
  procedure Handle_Errors is
   begin
      Put_Line("Invalid input. Please enter a valid row and column (1-3).");
   end Handle_Errors;













    -- Main program
   procedure Main is
       --placeholders below
      Board : Board_Type := ((' ', ' ', ' '), (' ', ' ', ' '), (' ', ' ', ' '));
      Current_Player : Player_Type := Player_X;
      Row, Col : Integer;
   begin
      -- main game loop here?
      loop
         -- Board Display placeholder
         Display_Board(Board);

         -- prompt the current player for their move
         Put_Line("Player " & Player'Image(Current_Player) & "'s turn:");
         Get_User_Input(Row, Col);

         -- Check if  move is valid
         if Valid_Move(Board, Row, Col) then
            -- Update the board with the player's move
            Board(Row, Col) := if Current_Player = Player_X then 'X' else 'O';

            -- Switch to the next player
            Current_Player := if Current_Player = Player_X then Player_O else Player_X;
         else
            -- If the move is invalid, display eror
            raise Invalid_Input;
         end if;
      end loop;
   exception
      when Invalid_Input =>
         Handle_Errors;
      -- **********************Handle other exceptions as needed************************
   end Main;

begin
   -- Error   program execution
   Main;
end TicTacToe;
