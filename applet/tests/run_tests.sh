#!/bin/bash
#
# Builds the library, the spawn server and the tests, then runs the tests, and reports any failures.
#
#    usage: run_tests.sh [--stress] [pattern]
#
#    pattern  : Only run the tests whose folder matches this extended regular expression.
#    --stress : Also run the tests which push against the system's limits.
#
# Exits with status 0 only if nothing failed. Each test ...
#
#   - is rebuilt, since building the library does not relink the programs which use it.
#   - uses the local spawn server, rather than an installed one.
#   - runs with SIGPIPE at its default, even if this shell ignores it, as child programs inherit that and behave differently.
#   - has a time limit, after which it and any processes it started are killed.
#   - fails on a bad exit status, an unhandled exception, a 'Fatal Error' from one of its tasks, or missing expected output.
#
# The output of a failed test is kept in a temporary folder, whose path is shown. 'core/pause_resume' lists the whole
# disk, so its output is around 100 MB.

Tests=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
Root=$(cd "$Tests/../.." && pwd)
Server=$Root/applet/spawn_server

Stress=false
Pattern=

for Each in "$@"
do
   case $Each in
      --stress) Stress=true ;;
      -*)       sed -n '3,6p' "${BASH_SOURCE[0]}"; exit 2 ;;
      *)        Pattern=$Each ;;
   esac
done

Output=$(mktemp -d)

export PATH=$Server:$PATH
export GPR_PROJECT_PATH=$Root/library:$GPR_PROJECT_PATH

Passed=0
Failures=()


function pass
{
   Passed=$((Passed + 1))
   printf 'PASS  %-48s %3ss\n' "$1" "$2"
}


function fail     # name reason [output file]
{
   Failures+=("$1")
   printf 'FAIL  %-48s %s\n' "$1" "$2"

   if [ -n "$3" ]
   then
      tail -n 10 "$3" | cut -c1-150 | sed 's/^/      | /'
      echo "      Full output: $3"
   fi
}


function selected
{
   [ -z "$Pattern" ] || [[ $1 =~ $Pattern ]]
}


function build     # folder project log
{
   (cd "$Root/$1" && gprbuild -p -P "$2" > "$3" 2>&1)
}


# Runs a program which is expected to pass, then checks its output for each of the given extended regular expressions.
#
#    check folder 'time limit' 'arguments' [expected output ...]
#
function check
{
   local Name=$1  Limit=$2  Arguments=$3
   shift 3

   selected "$Name" || return

   local Folder=$Tests/$Name
   local Project
   Project=$(basename "$(ls "$Folder"/*.gpr)" .gpr)

   local Log=$Output/${Name//\//_}.log

   if ! build "applet/tests/$Name" "$Project" "$Log"
   then
      fail "$Name" "does not build" "$Log"
      return
   fi

   local Start=$SECONDS
   local Status
   local Program
   Program=$(sed -n 's/.*for Main .*("\([^".]*\)[^"]*").*/\1/p' "$Folder/$Project.gpr")     # May differ from the project's name.

   # 'Timeout' runs the test in its own process group, and kills the whole group. So the
   # spawn server and any commands go too.
   #
   # shellcheck disable=SC2086
   (cd "$Folder" && env --default-signal=PIPE timeout -s KILL "$Limit" "./$Program" $Arguments) < /dev/null > "$Log" 2>&1
   Status=$?

   local Took=$((SECONDS - Start))

   if [ $Status = 137 ]
   then
      fail "$Name" "still running after the ${Limit}s limit" "$Log"
   elif [ $Status != 0 ]
   then
      fail "$Name" "exit status $Status" "$Log"
   elif grep -qE '^raised [A-Z_.]+( : |$)|^Task [0-9]+: Fatal Error' "$Log"     # Exact, as some tests list files, whose names could be anything.
   then
      fail "$Name" "an exception was raised" "$Log"
   else
      local Each
      for Each in "$@"
      do
         if ! grep -qE -- "$Each" "$Log"
         then
            fail "$Name" "expected output not found: '$Each'" "$Log"
            return
         fi
      done

      pass "$Name" "$Took"
      rm -f "$Log"
   fi
}


# Commands must still run when the spawn server can't create its log.
#
function check_without_writeable_folder
{
   local Name=commands/safe/no_input/read_only

   selected "$Name" || return

   local Folder
   local Log=$Output/${Name//\//_}.log
   local Start=$SECONDS
   local Status

   Folder=$(mktemp -d)
   chmod a-w "$Folder"

   (cd "$Folder" && env --default-signal=PIPE timeout -s KILL 30 "$Tests/commands/safe/no_input/test_no_input") < /dev/null > "$Log" 2>&1
   Status=$?

   chmod u+w "$Folder"
   rmdir "$Folder"

   if [ $Status = 137 ]
   then
      fail "$Name" "still running after the 30s limit" "$Log"
   elif [ $Status != 0 ]
   then
      fail "$Name" "exit status $Status" "$Log"
   else
      pass "$Name" $((SECONDS - Start))
      rm -f "$Log"
   fi
}


function check_builds     # folder ...
{
   local Each
   for Each in "$@"
   do
      selected "$Each" || continue

      local Folder=$Tests/$Each
      local Project
      Project=$(basename "$(ls "$Folder"/*.gpr)" .gpr)

      local Log=$Output/${Each//\//_}.log

      if build "applet/tests/$Each" "$Project" "$Log"
      then
         pass "$Each (build only)" 0
         rm -f "$Log"
      else
         fail "$Each (build only)" "does not build" "$Log"
      fi
   done
}


function check_unit_tests
{
   selected "unit" || return

   local Log=$Output/unit.log
   local Start=$SECONDS

   if ! build "library/test/unit" "test_all" "$Log"
   then
      fail "unit" "does not build" "$Log"
      return
   fi

   (cd "$Root/library/test/unit" && env --default-signal=PIPE timeout -s KILL 120 ./test_all) < /dev/null > "$Log" 2>&1

   # Ahven's exit status can't be relied on, so read its results.
   #
   if grep -qE '^Passed :' "$Log" && ! grep -qE '^(Failed|Errors) :|[[:space:]](FAIL|ERROR)[[:space:]]' "$Log"
   then
      pass "unit ($(grep -E '^Passed :' "$Log" | tr -d ' ' | cut -d: -f2) tests)" $((SECONDS - Start))
      rm -f "$Log"
   else
      fail "unit" "Ahven reported failures, or did not finish" "$Log"
   fi
}


# The library and server must build, or every result would be meaningless.
#
Log=$Output/library.log
if ! build library ashell "$Log"
then
   fail "library" "does not build" "$Log"
   exit 1
fi

Log=$Output/spawn_server.log
if ! build applet/spawn_server spawn_server "$Log"
then
   fail "spawn server" "does not build" "$Log"
   exit 1
fi


check_unit_tests

check core/piped_processes           30 ''   '3 b'  'Success'
check core/pipe_output_to_string     60 ''   "End 'Pipe_Output_To_String'"
check core/wait_on_process           30 ''   "End 'Wait_On_Process'"
check core/process_status            60 ''   "End +'Test_Process_Status'"
check core/pause_resume             120 ''   "End 'Test_Pause_Resume'"

check directories                    30 ''   'test_iterate_directory\.adb'  "End 'Iterate_Directory'"

check commands/unsafe/close_input               60 ''   'Sending to a program which has gone raises passed'
check commands/unsafe/command_error             30 ''   "Failed on command .* as expected"  'raised an exception, as expected'
check commands/unsafe/command_input             30 ''   'Hello world'  "End Test_Command_Input"
check commands/unsafe/command_output_to_string  30 ''   'test_command_output_to_string\.adb'
check commands/unsafe/command_status            60 ''   "End +'Test_Unsafe_Command_Status'"
check commands/unsafe/environment               30 ''   'Actual   output: aShell_Test_Variable=Working'  'Actual   output: aShell_Test_Variable=Changed'
check commands/unsafe/large_input               60 ''   'Program which reads nothing passed'
check commands/unsafe/pipeline_error            60 ''   'Pipeline failed as expected'  'raised an exception, as expected'  "End 'Pipeline_Error'"
check commands/unsafe/pipeline_output           30 ''   "End 'Pipeline_Output'"
check commands/unsafe/wait_on                   60 ''   'Waiting on a small pipeline passed'
check commands/unsafe/pipeline_failure          60 ''   'End test'
check commands/unsafe/start_failure             60 ''   'End test'
check commands/unsafe/unused_command            30 ''   'Success'

check commands/safe/close_input                 60 ''      "Other commands aren't held up passed"
check commands/safe/command_failure             60 ''      'Server still runs commands passed'
check commands/safe/command_input               30 ''      'Goodbye world'  'End Test_Command_Input'
check commands/safe/command_status              60 ''      "End +'Test_Safe_Command_Status'"
check commands/safe/concurrent_commands        120 '100'   'Main Task has terminated'
check commands/safe/concurrent_pipelines       120 '50'    "End 'Concurrent_Pipelines'"
check commands/safe/exit_with_input             30 ''      'Main ending'
check commands/safe/late_kill                   15 ''      'Main ending'
check commands/safe/no_input                    30 ''      'Waiting on a command which failed to start raises passed'
check_without_writeable_folder
check commands/safe/large_input                 60 ''      'Program which reads nothing passed'
check commands/safe/pipeline_output             60 'quick' "End 'Pipeline_Output'"
check commands/safe/server_lost                 60 ''      "'Stop_Spawn_Client' returns passed"
check commands/safe/spawn_server                60 '50'    'End test'
check commands/safe/unused_command              30 ''      'Success'
check all_safe_command_tests                   240 '20'    'End all safe command tests'

if $Stress
then
   check stress/max_pipes  120 ''  'pipes'
fi

# These wait for a person, or for input. So only check that they still build.
#
check_builds all_core_tests  all_unsafe_command_tests  commands/unsafe/ashell  commands/interactive/nonblocking_pipe

echo
echo "Passed: $Passed   Failed: ${#Failures[@]}"

if [ ${#Failures[@]} != 0 ]
then
   printf '   %s\n' "${Failures[@]}"
   exit 1
fi

rm -rf "$Output"
