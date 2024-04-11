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
            Close (File);
            Put_Line ("Data file did not exist. New file created.");
            New_Line;
            Put_Line ("Program terminating. Please Restart.");
            GNAT.OS_Lib.OS_Exit (0);

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

end accounts;
