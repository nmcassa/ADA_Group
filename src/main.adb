with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with accounts;

procedure MAIN is
   --  create an index for our board array from (0 - 8)
   type Index is range 0 .. 8;
   type Board is array (Index) of Character;

   B : Board := ('-', '-', '-',
                 '-', '-', '-',
                '-', '-', '-');

   type IntroBoard is array (Index) of Integer;

   Intro : constant IntroBoard := (0, 1, 2, 3, 4, 5, 6, 7, 8);

   Temp : Integer;

   Winner : Character;

   EndGame : Boolean := False;

   EndingString : Unbounded_String;

   function IntroPrint (Intro : IntroBoard) return Integer
   is
   begin
      Put_Line ("When prompted, select the space you want to make your move.");
      for I in Index loop
         if I = 3 or else I = 6 then
            New_Line;
         end if;
         Put (Integer'Image (Intro (I)));
      end loop;
      New_Line;
      return 1;

   end IntroPrint;

   function DisplayBoard (B : Board) return Integer
   is
   begin
      for I in Index loop
         if I = 3 or else I = 6 then
            New_Line;
         end if;
         Put (B (I));
      end loop;
      return 1;
   end DisplayBoard;

   function PlayMove (B : out Board; I : Index; Player : Character)
                      return Integer is
      Temp : Integer;
      New_Move : Index;
   begin
      --  Error Check if I is outside of 0 .. 8
      if B (I) = '-' then
         B (I) := Player;
         return 1;
      end if;
      New_Line;
      Put ("That space is already taken, select a new space: ");
      New_Move := Index'Value (Get_Line);
      New_Line;
      Temp := PlayMove (B, New_Move, Player);
      return 1;
   end PlayMove;

   function CheckWin (B : Board; Player : Character) return Integer
   is
   begin
      for I in 0 .. 2 loop
         --  check rows
         if B (Index (I * 3 + 0)) = Player and then
           B (Index (I * 3 + 1)) = Player and then
           B (Index (I * 3 + 2)) = Player
         then

            return 1;

         end if;

         --  check cols
         if B (Index (0 * 3 + I)) = Player and then
           B (Index (1 * 3 + I)) = Player and then
           B (Index (2 * 3 + I)) = Player
         then

            return 1;

         end if;

      end loop;

      --  check diag
      if B (Index (0)) = Player and then
        B (Index (4)) = Player and then
        B (Index (8)) = Player
      then

         return 1;

      end if;

      --  check other diag
      if B (Index (2)) = Player and then
        B (Index (4)) = Player and then
        B (Index (6)) = Player
      then

         return 1;

      end if;

      --  0 if not winning board
      return 0;
   end CheckWin;

   function CheckTie (B : Board) return Integer
   is
   begin

      for I in 0 .. 8 loop
         if B (Index (I)) = '-' then
            return 0;
         end if;
      end loop;
      return 1;

   end CheckTie;

   function GameLoop (B : out Board) return Character
   is
      looping : Integer;
      Move : Index;
      Temp : Integer;
   begin
      looping := 1;

      New_Line;
      Temp := DisplayBoard (B);
      New_Line;

      while looping = 1 loop
         begin
            --  PlayerX's move
            New_Line;
            Put_Line (To_String (accounts.userData.Username) & " - Make a Move: ");

            Move := Index'Value (Get_Line);
            Temp := PlayMove (B, Move, 'X');

            New_Line;
            Temp := DisplayBoard (B);
            New_Line;

            --  Check if X won
            --  if X won, return X
            if CheckWin (B, 'X') = 1 then
               return 'X';
            end if;

            --  Check if tie
            if CheckTie (B) = 1 then
               return '-';
            end if;

            --  PlayerO's move
            New_Line;
            Put ("Player O - Make a Move: ");

            Move := Index'Value (Get_Line);
            Temp := PlayMove (B, Move, 'O');

            if CheckWin (B, 'O') = 1 then
               return 'O';
            end if;

            --  Check if tie
            if CheckTie (B) = 1 then
               return '-';
            end if;

            New_Line;
            Temp := DisplayBoard (B);
            New_Line;
         exception
            when Constraint_Error =>
               Put_Line ("You entered an invalid input. Try again.");
         end;
      end loop;
      return 'X';

   end GameLoop;

begin
   accounts.AccountLogin;

   while not EndGame loop
      Temp := IntroPrint (Intro);
      Winner := GameLoop (B);

      New_Line;
      Temp := DisplayBoard (B);
      New_Line;

      if Winner = 'X' then
         New_Line;
         Put_Line (To_String (accounts.userData.Username) & " won the game!");
         accounts.userData.Wins := accounts.userData.Wins + 1;
      end if;

      if Winner = 'O' then
         New_Line;
         Put ("Player O won the game!");
         New_Line;
         accounts.userData.Losses := accounts.userData.Losses + 1;
      end if;

      if Winner = '-' then
         New_Line;
         Put ("No one wins");
         accounts.userData.Draws := accounts.userData.Draws + 1;
         New_Line;
      end if;
      
      B := ('-', '-', '-',
            '-', '-', '-',
           '-', '-', '-');
      
      Put_Line ("Enter P to play again. Enter Q to quit the game");
      EndingString := To_Unbounded_String (Get_Line);

      if Ada.Characters.Handling.To_Lower (To_String (EndingString)) = "p" then
         Put_Line ("Next Game!");

      elsif To_Lower (To_String (EndingString)) = "q" then
         Put_Line ("Program terminating");
         EndGame := True;
      else
         Put_Line ("Invalid input. Game will continue");
      end if;
   end loop;

   accounts.saveData;
end MAIN;
