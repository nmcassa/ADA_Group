with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package accounts is

   procedure saveData;

   type userDataArray is array (Integer range 1 .. 3) of Integer;

   function loadData (username : Unbounded_String; password : Unbounded_String)
                      return userDataArray;

   --  Record type declaration to store player info.
   type gameRecord is record

      Username : Unbounded_String;
      Password : Unbounded_String;
      Wins : Integer;
      Losses : Integer;
      Draws : Integer;

   end record;

   --  Tracks the line in the file a user's data is on. Used for saving.
   fileLinePosition : Integer := 1;

   --  Instantiates the record to store player info.
   userData : gameRecord := (To_Unbounded_String ("Guest"),
                             To_Unbounded_String ("12345"),
                             0, 0, 0);

   --  Used in AccountLogin procedure to verify if login was working.
   loginResult : accounts.userDataArray;

end accounts;
