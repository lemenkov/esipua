-record(command,{type,id,retvalue,header,keys}).

%% command header records
-record(message,{time,name,processed}).
-record(install,{priority,filter}).
-record(uninstall,{priority}).
-record(watch,{}).
-record(unwatch,{}).
-record(setlocal,{value}).
-record(output,{str}).
-record(connect,{role,type}).
