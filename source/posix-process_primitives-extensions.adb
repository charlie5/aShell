------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--              P O S I X . P R O C E S S _ P R I M I T I V E S             --
--                               .Extensions                                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2006 AdaCore                      --
--               Copyright (C) 2017 JSA Research & Innovation               --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5: 1990  and IEEE STD  --
--  1003.5b: 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories;

with POSIX.Implementation,
     POSIX.Unsafe_Process_Primitives,
     Unchecked_Conversion;

package body POSIX.Process_Primitives.Extensions is

   use POSIX.C,
       POSIX.Implementation,
       POSIX.Process_Identification,
       POSIX.Process_Environment;

   C_File_Mode : constant array (POSIX.IO.File_Mode) of Bits :=
     (POSIX.IO.Read_Only  => O_RDONLY,
      POSIX.IO.Write_Only => O_WRONLY,
      POSIX.IO.Read_Write => O_RDWR);

   -----------------------------
   --  Unchecked Conversions  --
   -----------------------------

   function To_int is new Unchecked_Conversion (Bits, int);

   function To_String_List_Ptr is
     new Unchecked_Conversion (POSIX_String_List, String_List_Ptr);

   function To_Process_ID is new
      Unchecked_Conversion (pid_t, Process_ID);
   function To_pid_t is new
      Unchecked_Conversion (Process_ID, pid_t);

   -------------------------
   --  Local Subprograms  --
   -------------------------

   procedure Execute_Template (Template : Process_Template);

   procedure Void (Ignore : int);
   pragma Inline (Void);
   procedure Void (Ignore : int) is
      pragma Unreferenced (Ignore);
   begin
      null;
   end Void;

   function sigemptyset (set : sigset_t_ptr) return int;
   pragma Import (C, sigemptyset, sigemptyset_LINKNAME);
   function sigaddset (set : sigset_t_ptr; sig : POSIX.Signals.Signal)
     return int;
   pragma Import (C, sigaddset, sigaddset_LINKNAME);
   function pthread_sigmask
     (how : int;
      set : sigset_t_ptr;
      oset : sigset_t_ptr) return int;
   pragma Import (C, pthread_sigmask, pthread_sigmask_LINKNAME);

   procedure Check_Fatal (Result : int);
   --  See comments in Execute_Template, below.
   procedure Check_Fatal (Result : int) is
   begin
      if Result = -1 then
         Exit_Process (Failed_Creation_Exit);
      end if;
   end Check_Fatal;

   function getuid return uid_t;
   pragma Import (C, getuid, getuid_LINKNAME);
   function setuid (uid : uid_t) return int;
   pragma Import (C, setuid, setuid_LINKNAME);

   function getgid return gid_t;
   pragma Import (C, getgid, getgid_LINKNAME);
   function setgid (gid : gid_t) return int;
   pragma Import (C, setgid, setgid_LINKNAME);

   function close (fildes : int) return int;
   pragma Import (C, close, close_LINKNAME);

   function open (path : char_ptr; oflag : int) return int;
   pragma Import (C, open, open_LINKNAME);

   function dup2 (fildes, fildes2 : int) return int;
   pragma Import (C, dup2, dup2_LINKNAME);

   procedure Execute_Template (Template : Process_Template) is
      FD1, FD2 : int;
      Cur : FD_Set_Ptr := Template.FD_Set;
      New_Mask, Old_Mask : aliased sigset_t;

   begin
      if not Template.Keep_Effective_IDs then
         --  See note below why we do not call operations from
         --  POSIX_Process_Identification, since they may raise
         --  exceptions, and we worry about our ability to handle
         --  them.
         Check_Fatal (setuid (getuid));
         Check_Fatal (setgid (getgid));
      end if;

      --  We cannot use signal masking operations from
      --  POSIX.Signals, since they are implemented as
      --  virtual operations, relative to the Ada task's
      --  view of the signal interface.  We normally keep
      --  most signals masked in all tasks except the designated
      --  signal handler threads, so that we can safely use
      --  sigwait.  During this situation, we have just forked
      --  and we hope|expect there are no other threads active
      --  in the new (child) process.  Under these conditions
      --  (only) it should be safe to use the raw signal masking
      --  operations.  In earlier versions, we used the almost-raw
      --  versions, from System.Interrupt_Management.Operations.
      --  These had the advantage that the Ada RTS has already
      --  taken care of mapping to any nonstandard functions,
      --  such as the Solaris 2.x thr_sigmask, versus the
      --  POSIX.1c pthread_sigmask.  However, more recent versions
      --  of Unix operating systems do support the standard,
      --  and in posi-signals.gpb we have already used some of
      --  the raw C interfaces.  In the current version, we have
      --  gone over to completely avoiding calls to the Ada tasking
      --  runtime system.

      --  If an exception is raised during this time, the tasking
      --  runtime system's data structures may "lie" about there
      --  being other tasks active.  This could prevent
      --  orderly shutdown of the process.  Hence, we use
      --  Check_Fatal instead of the usual Check, and generally
      --  try to avoid calling anything that could raise an
      --  exception.

      --  .... ????
      --  The code below may not be robust against exceptions
      --  that occur between fork and exec calls.  There may be
      --  a possibility of deadlock, if the fork occurred while some
      --  other task is holding an RTS-internal lock that we need to
      --  process exceptions.
      --  The present approach is to avoid exceptions, by calling the
      --  "raw" C interfaces below, and to replace the soft-links that are
      --  used to set up exception-handling frames to use the nontasking
      --  versions, since we may not be able to avoid those routines being
      --  called.  The soft links are switched inside the version of Fork
      --  that we import from POSIX.Unsafe_Process_Primitives.

      while Cur /= null loop
         case Cur.Action is
         when Close =>
            Check_Fatal (close (int (Cur.FD)));
         when Open  =>
            FD1 := open (Cur.File_Name (Cur.File_Name'First)'Unchecked_Access,
              To_int (Option_Set (Cur.File_Options).Option
                or C_File_Mode (Cur.File_Mode)));
            if FD1 = -1 then
               Exit_Process (Failed_Creation_Exit);
            end if;
            --  FD2 := dup2 (FD1, int (Cur.FD)); should be enough for the
            --  following if/else statement. However, we have a mulfunction
            --  under Linux when the two arguments are the same. The following
            --  code is a workaround.
            if FD1 /= int (Cur.FD) then
               FD2 := dup2 (FD1, int (Cur.FD));
            else
               FD2 := FD1;
            end if;
            if FD2 = -1 then
               Exit_Process (Failed_Creation_Exit);
            end if;
         when Duplicate =>
            FD2 := dup2 (int (Cur.Dup_From), int (Cur.FD));
            if FD2 = -1 then
               Exit_Process (Failed_Creation_Exit);
            end if;
         end case;
         Cur := Cur.Next;
      end loop;
      Void (sigemptyset (New_Mask'Unchecked_Access));
      for Sig in 1 .. POSIX.Signals.Signal'Last loop
         if POSIX.Signals.Is_Member (Template.Sig_Set, Sig) then
            Void (sigaddset (New_Mask'Unchecked_Access, Sig));
         end if;
      end loop;
      Void (pthread_sigmask (SIG_SETMASK, New_Mask'Unchecked_Access,
        Old_Mask'Unchecked_Access));
      --  ???? is pthread_sigmask OK after a fork?
      --  sigprocmask is not safe in a multithreaded process, but after
      --  the fork() call we are effectively in a single-threaded process,
      --  so it might be better to use sigprocmask?
      --  Void (sigprocmask (SIG_SETMASK, New_Mask'Unchecked_Access, null));
   exception when others =>
      Exit_Process (Failed_Creation_Exit);
   --  Since this may not work, we have tried to avoid raising
   --  any exceptions.  However, in case we have missed something
   --  and an exception is raised, we leave the handler here,
   --  on the off-chance it might work.
   end Execute_Template;

   procedure Validate (Template : Process_Template);

   procedure Validate (Template : Process_Template) is
   begin
      if Template.Is_Closed then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
   end Validate;

   ---------------------
   --  Start_Process  --
   ---------------------

   function execvp
     (file : char_ptr;
      argv : char_ptr_ptr) return int;
   pragma Import (C, execvp, execvp_LINKNAME);

   function UFork return POSIX.Process_Identification.Process_ID
     renames POSIX.Unsafe_Process_Primitives.Fork;

   ----------------------------
   --  Start_Process_Search  --
   ----------------------------

   procedure Start_Process_Search
     (Child             :    out POSIX.Process_Identification.Process_ID;
      Filename          : in     POSIX.Filename;
      Working_Directory : in     String;
      Template          : in     Process_Template;
      Arg_List          : in     POSIX.POSIX_String_List :=
                                   POSIX.Empty_String_List)
   is
      pid : pid_t;
      Result : int;
      pragma Unreferenced (Result);

      Filename_With_NUL : POSIX_String := Filename & NUL;
      Arg : String_List_Ptr := To_String_List_Ptr (Arg_List);

   begin
      if Arg = null then
         Arg := Null_String_List_Ptr;
      end if;
      Validate (Template);
      pid := To_pid_t (UFork); Check (int (pid));
      if pid = 0 then    --  child process
         Ada.Directories.Set_Directory (Working_Directory);
         Execute_Template (Template);
         Result := execvp (Filename_With_NUL
           (Filename_With_NUL'First)'Unchecked_Access, Arg.Char (1)'Access);
         Exit_Process (Failed_Creation_Exit);
      else
         Child := To_Process_ID (pid);
      end if;
   end Start_Process_Search;

end POSIX.Process_Primitives.Extensions;
