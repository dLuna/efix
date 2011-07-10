%% -*- erlang-indent-level: 2 -*-
{application, fix,
 [{description, "Financial Information eXchange parser"},
  {vsn, "0.2"},
  {modules, [fix, fix_generate, fix_read_data,
             fix40, fix41, fix42, fix43, fix44, fix50, fix50sp1, fix50sp2, fixt11
            ]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
