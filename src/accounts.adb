with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.String_Split; use GNAT.String_Split;
with GNAT.OS_Lib;

package body accounts is

   --  Loads the user's data from the game file.
   function loadData (username : Unbounded_String; password : Unbounded_String)
                      return userDataArray is

      UserFromFile : Unbounded_String;
      PasswordFromFile : Unbounded_String;
      File : File_Type;
      LineFromFile : GNAT.String_Split.Slice_Set;
      dataResult : userDataArray;
      iterator : Integer := 0;

   begin

      begin

         Open (File, In_File, "userData.txt");

         while not End_Of_File (File) loop

            --  keeps track of the index of the line currently being read.
            iterator := iterator + 1;

            --  Uses the GNAT.String_Split lib to grab the user and password
            --  from the current line being read.
            --  |@@ is the seperator used in the file.
            --  I did it like this so a user could enter spaces in a username
            --  or password if they really wanted to without breaking the game.
            LineFromFile := Create (Get_Line (File), "|", Single);
            UserFromFile := To_Unbounded_String (Slice (LineFromFile, 1));
            PasswordFromFile := To_Unbounded_String (Slice (LineFromFile, 2));

            --  Checks the entered information against the data file
            --  If matching, the user's data is pulled from the rest of the
            --  line in the file and stored in an array (dataResult)
            --  that is returned by the function.
            if username = UserFromFile and then password = PasswordFromFile
            then
               dataResult (1) := Integer'Value (Slice (LineFromFile, 3));
               dataResult (2) := Integer'Value (Slice (LineFromFile, 4));
               dataResult (3) := Integer'Value (Slice (LineFromFile, 5));
               fileLinePosition := iterator;
               exit;
            else
               --  These values are just random placeholders I use
               --  to verify that the login wasn't found in the file.
               dataResult (1) := -777;
               fileLinePosition := -1;
            end if;

         end loop;

      exception
            --  Name_Error is thrown if no file with the name "userData.txt"
            --  is found. It will create the file, enter an "admin" account
            --  so that the file isn't blank, and terminates the program to
            --  prevent any unexpected errors from happening after this.

         when Name_Error =>
            Create (File, Out_File, "userData.txt");
            Put_Line (File, "Admin|12345|99|0|0");
            Put_Line ("Data file did not exist. New file created.");
            New_Line;
            dataResult (1) := -776;
            fileLinePosition := -1;

            --  Any other error thrown will be from attempting to read
            --  and parse existing data, so it's likely the data file was
            --  manipulated outside of the program.
         when others =>
            Put_Line ("There was an error reading data from the file." &
                        " File may be corrupted.");
            Put_Line ("Program terminating." &
                        "Please check data file and restart.");
            GNAT.OS_Lib.OS_Exit (0);
      end;

      Close (File);

      return dataResult;
   end loadData;

   --  Saves the user's data to file.
   procedure saveData is

      File : File_Type;
      TempFile : File_Type;
      fileLineCounter : Integer := 0;

   begin
      --  If the current user data isn't in the file ( = -1 ), it will append
      --  their data to the end of the file.
      --  Otherwise it will use the linePosition to replace the line with the
      --  old data.

      if accounts.fileLinePosition = -1 then
         Open (File, Append_File, "userData.txt");
         Put_Line (File, To_String (userData.Username) & "|" &
                     To_String (userData.Password) & "|" &
                     Trim (userData.Wins'Image, Ada.Strings.Both) & "|" &
                     Trim (userData.Draws'Image, Ada.Strings.Both) & "|" &
                     Trim (userData.Losses'Image, Ada.Strings.Both));
         Close (File);
      else
         --  Opens main data file and creates a temporary file to store data.
         Open (File, In_File, "userData.txt");
         Create (TempFile, Out_File, "userDataBackup.txt");

         --  This transfer all the data in the main file to the backup.
         while not End_Of_File (File) loop
            Put_Line (TempFile, Get_Line (File));
         end loop;

         --  Closes both files so I can change them from Read <-> Write.
         Close (File);
         Close (TempFile);

         Open (File, Out_File, "userData.txt");
         Open (TempFile, In_File, "userDataBackup.txt");

         --  Will transfer each line over from the backup except
         --  the old data line for the current user.

         while not End_Of_File (TempFile) loop

            --  Tracks line index of the TempFile
            fileLineCounter := fileLineCounter + 1;

            --  If the current line in Tempfile isn't the same line
            --  as the current user's data, it put the tempfile line directly
            --  into the main data file.
            if fileLineCounter /= accounts.fileLinePosition then
               Put_Line (File, Get_Line (TempFile));

               --  If the current line in tempfile is the same line as
               --  the current user's data, it writes their data stored
               --  in the data record to file, and then skips the line in
               --  tempfile to avoid having a dupe of the data line.
            elsif fileLineCounter = accounts.fileLinePosition then
               Put_Line (File, To_String (userData.Username) & "|" &
                           To_String (userData.Password) & "|" &
                           Trim (userData.Wins'Image, Ada.Strings.Both)
                         & "|" &
                           Trim (userData.Draws'Image, Ada.Strings.Both)
                         & "|" &
                           Trim (userData.Losses'Image, Ada.Strings.Both));

               Skip_Line (File => TempFile);

            end if;

         end loop;

         Close (File);
         Delete (TempFile);

      end if;

   end saveData;

   --  Procedure that executes the login/signup loop.
   procedure AccountLogin is

      Complete : Boolean := False;

   begin

      while not Complete loop

         --  Prompts the user for their login
         Put_Line ("Please enter your username and press enter.");

         accounts.userData.Username := To_Unbounded_String
           (Trim (Get_Line, Ada.Strings.Both));
         New_Line;

         Put_Line ("Please enter your password and press enter.");
         accounts.userData.Password := To_Unbounded_String
           (Trim (Get_Line, Ada.Strings.Both));
         New_Line;

         --  Calls the loadData function with the username and password
         --  that was entered. loginResult holds the array of data
         --  pulled from the file. Either it will actually hold the user's
         --  playing record, or the first index will hold -777
         --  meaning the user has a new account and there's no existing data
         accounts.loginResult := accounts.loadData
           (accounts.userData.Username,
            accounts.userData.Password);

         if accounts.loginResult (1) = -776 then
            Put_Line ("The following information is your account login data.");

            Put_Line (ASCII.HT &
                        "Entered Username: "
                      & To_String (accounts.userData.Username));

            Put_Line (ASCII.HT &
                        "Entered Password: "
                      & To_String (accounts.userData.Password));
            Complete := True;
            New_Line;

            Put_Line ("Hit Enter to continue.");
            Put (Get_Line);

         elsif accounts.loginResult (1) = -777 then
            --  Allows the user to create a login with the info they entered
            --  or allows them to either re-enter a login or quit the game.

            --  There's no built in escape sequence like \t so you
            --  either have to create your own or use the ugly ASCII lib:sob:
            Put_Line ("The login entered was not found.");

            Put_Line ("Would you like to re-enter your login or sign up with" &
                        " the current login?");

            Put_Line (ASCII.HT &
                        "Entered Username: "
                      & To_String (accounts.userData.Username));

            Put_Line (ASCII.HT &
                        "Entered Password: "
                      & To_String (accounts.userData.Password));
            Put_Line ("Enter Y to create this account, N to retry login" &
                        ", or Q to exit the game.");

            if Ada.Characters.Handling.To_Lower (Get_Line) = "y" then
               Complete := True;
            elsif To_Lower (Get_Line) = "q" then
               Put_Line ("Program terminating");
               GNAT.OS_Lib.OS_Exit (0);
            end if;

         elsif accounts.loginResult (1) /= -777 then

            Put_Line ("Welcome back, " & To_String
                      (accounts.userData.Username) & "!");
            accounts.userData.Wins := accounts.loginResult (1);
            accounts.userData.Draws := accounts.loginResult (2);
            accounts.userData.Losses := accounts.loginResult (3);
            Put_Line ("Your game record is "
                      & accounts.userData.Wins'Image & " -"
                      & accounts.userData.Draws'Image
                      & " -" & accounts.userData.Losses'Image &
                        "!");
            Put_Line ("Hit Enter to continue.");
            Put (Get_Line);
            Complete := True;

         end if;
      end loop;
   end AccountLogin;

end accounts;
