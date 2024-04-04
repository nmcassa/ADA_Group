with Ada.Text_IO;

procedure ScoreTracking_BackEnd is
   -- Define a record type to hold player scores
   type PlayerScores is record
      Wins   : Natural := 0;
      Losses : Natural := 0;
      Draws  : Natural := 0;
   end record;

   -- Define a record type to hold scores for both human and AI opponents
   type Scoreboard is record
      HumanPlayer : PlayerScores;
      AIPlayer    : PlayerScores;
   end record;

   -- Procedure to initialize scores
   procedure InitializeScores(out Board : Scoreboard) is
   begin
      Board.HumanPlayer := (Wins => 0, Losses => 0, Draws => 0);
      Board.AIPlayer := (Wins => 0, Losses => 0, Draws => 0);
   end InitializeScores;

   -- Procedure to update scores for a win
   procedure UpdateWin(out Board : Scoreboard; PlayerType : Integer) is
   begin
      case PlayerType is
         when 1 =>
            Board.HumanPlayer.Wins := Board.HumanPlayer.Wins + 1;
            Board.AIPlayer.Losses := Board.AIPlayer.Losses + 1;
         when 2 =>
            Board.AIPlayer.Wins := Board.AIPlayer.Wins + 1;
            Board.HumanPlayer.Losses := Board.HumanPlayer.Losses + 1;
         when others =>
            null; -- Handle invalid player type
      end case;
   end UpdateWin;

   -- Procedure to update scores for a draw
   procedure UpdateDraw(out Board : Scoreboard) is
   begin
      Board.HumanPlayer.Draws := Board.HumanPlayer.Draws + 1;
      Board.AIPlayer.Draws := Board.AIPlayer.Draws + 1;
   end UpdateDraw;

   -- Procedure to display scores
   procedure DisplayScores(Board : Scoreboard) is
   begin
      Ada.Text_IO.Put_Line("Human Player Scores:");
      Ada.Text_IO.Put_Line("Wins:   " & Natural'Image(Board.HumanPlayer.Wins));
      Ada.Text_IO.Put_Line("Losses: " & Natural'Image(Board.HumanPlayer.Losses));
      Ada.Text_IO.Put_Line("Draws:  " & Natural'Image(Board.HumanPlayer.Draws));

      Ada.Text_IO.Put_Line("AI Player Scores:");
      Ada.Text_IO.Put_Line("Wins:   " & Natural'Image(Board.AIPlayer.Wins));
      Ada.Text_IO.Put_Line("Losses: " & Natural'Image(Board.AIPlayer.Losses));
      Ada.Text_IO.Put_Line("Draws:  " & Natural'Image(Board.AIPlayer.Draws));
   end DisplayScores;

   -- Example usage
   Board : Scoreboard;
begin
   -- Initialize scores
   InitializeScores(Board);

   -- Simulate game outcomes
   UpdateWin(Board, 1); -- Human wins
   UpdateWin(Board, 2); -- AI wins
   UpdateDraw(Board);   -- Draw

   -- Display scores
   DisplayScores(Board);
end ScoreTracking_BackEnd.
