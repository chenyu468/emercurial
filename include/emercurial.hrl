
-define(R2P(Record), record_to_proplist(#Record{} = Rec) ->
           lists:zip(record_info(fields, Record), tl(tuple_to_list(Rec)))).

-define(REVISION,[rev,
                  node,
                  tags,
                  branch,
                  author,
                  desc,
                  date
                  ]).

-record(clone,{source=none,
               dest = none,
               noupdate = false,
               updaterev = none,
               rev = none,
               branch = none,
               pull = false,
               uncompressed = false,
               e = none, %%≤ª π”√ssh
               remotecmd = none,
               insecure = false,
               encoding = none,
               configs = none}).


-record(raw_command,{args,
                    error_handler=none,
                    prompt=none,
                    input=none
                    }).

-record(mercurial_command_error,{args,
                         ret,
                         out,
                         error}).

-record(mercurial_value_error,{value}).

-record(log,{revrange=none,
             files=[],
             follow=false,
             follow_first=false,
             date=none,
             copies=false,
             keyword=none,
             removed=false,
             onlymerge=false,
             user=none,
             branch=none,
             prune=none,
             hidden=false,
             limit=none,
             nomerge=false,
             include=none,
             exclude=none}).



-record(branch,{name = none,
                clean = false,
                force = false}).



-record(commit,{message = none,
                log_file = none,
                add_remove = false,
                close_branch = false,
                date = none,
                user = none,
                include = none,
                exclude = none}).

-record(internal_commit,{debug = true,
                         m, % message
                         'A', % addremove
                         close_branch, %close_branch
                         d, % date
                         u, % user
                         l, % logfile
                         'I', % include
                         'X' % exculde
                         }).

-record(update,{rev = none,
                clean = false,
                check = false,
                date = none
                }).

-record(internal_update,{r, %% rev
                         'C', %% clean
                         c, %% check
                         d %% date
                         }).

-record(parents,{rev=none,
                file=none
                }).



-record(cat,{files,
             rev=none,
             output=none
             }).

-record(internal_cat,{r,
                      o
                      }).

-record(diff,{files=[],
              revs = [],
              change = none,
              text = false,
              git = false,
              nodates = false,
              showfunction = false,
              reverse = false,
              ignoreallspace = false,
              ignorespacechange = false,
              ignoreblanklines = false,
              unified = none,
              stat = false,
              subrepos = false,
              include = none,
              exclude = none
              }).


-record(add,{files = [],
             dryrun = false,
             subrepos = false,
             include = none,
             exclude = none
             }).

-record(push,{dest=none,rev=none,force=false,bookmark=none,branch=none,
              newbranch=false,ssh=none,remotecmd=none,insecure=false}).

-record(tag,{names,rev=none,message=none,force=false,local=false,
             remove=false,date=none,user=none}).

-record(internal_tag,{r,
                      m,
                      f,
                      l,
                      remove,
                      d,
                      u,
                      names}).

-record(internal_add,{n, %% dryrun
                      'S', %% subrepos
                      'I', %% include
                      'X' %% exclude
                      }).

-record(internal_branch,{name,
                         f,
                         'C'}).


-record(internal_clone,{b, %% branch
                        u, %% updaterev
                        r %% revrange
                        }).

-record(internal_log,{template,          
          r, %%revrange
                      f, %%follow
                      follow_first,
                      d, %%date
                      c, %copies
                      k, %keyword
                      removed,%removed
                      m, %onlymerges
                      u, % user
                      b, %branch
                      'P', %prune
                      h, %hidden
                      l, %%limit
                      'M',%nomerges
                      'I',%include
                      'X' %exclude
                      }).

-record(internal_parents,{template,
                r
                }).

-record(internal_push,{dest,
                       r,
                       f,
                       'B',
                       b,
                       new_branch,
                       e,
                       remotecmd,
                       insecure
                       }).

-record(internal_diff,{r, %% revs
                       c, %% change
                       a, %% text
                       g, %% git
                       nodates, %% nodates
                       p, %% showfunction
                       reverse, %% reverse
                       w, %% ignoreallspace
                       b, %% ignorespacechange
                       'B', %% ignoreblanklines
                       'U', %% unified
                       stat, %% stat
                       'S', %% subrepos
                       'I', %% include
                       'X' %% exclude
                       }).

-record(internal_tags,{v
                       }).
