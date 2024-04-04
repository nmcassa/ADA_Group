with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;

procedure Projecttwo is

   loginResult : Boolean;
   fileLinePosition : Natural;

   function loadData (username : Unbounded_String; password : Unbounded_String)
                      return Boolean is

      Result : Boolean := False;
      UserFromFile : Unbounded_String;
      PasswordFromFile : Unbounded_String;
      File : File_Type;
      LineFromFile : GNAT.String_Split.Slice_Set;

   begin

      declare

         counter : Natural := 0;

      begin

         Open (File, In_File, "userData.txt");
         while not End_Of_File (File) loop

            counter := counter + 1;

            LineFromFile := Create (Get_Line (File), " ", Single);
            UserFromFile := To_Unbounded_String (Slice (LineFromFile, 1));
            Put_Line (To_String (UserFromFile));
            PasswordFromFile := To_Unbounded_String (Slice (LineFromFile, 2));

            if username = UserFromFile and then password = PasswordFromFile
            then
               Result := True;
               fileLinePosition := counter;
               exit;
            end if;

         end loop;

      exception
         when Name_Error =>
            Create (File, Out_File, "userData.txt");
            Put_Line ("Data file did not exist. New file created."  &
                     " No data was loaded");
            Result := False;
         when others =>
            Put_Line ("There was an error reading data from the file." &
                        " File may be corrupted.");
      end;

      return Result;
   end loadData;

   function saveData (username : Unbounded_String; password : Unbounded_String) 
                      return Boolean is

      Result : Boolean := False;
      UserFromFile : Unbounded_String;
      PasswordFromFile : Unbounded_String;
      File : File_Type;
      LineFromFile : GNAT.String_Split.Slice_Set;

   begin
      Put_Line (" ");
      return False;
   end saveData;

begin
   loginResult := loadData (To_Unbounded_String ("Roger"),
                            To_Unbounded_String ("King"));

   if loginResult then
      Put_Line ("Login was successful");
      Put_Line ("Line in Data File : " & fileLinePosition'Image);
   else
      Put_Line ("The login entered was incorrect");
   end if;

end Projecttwo;
