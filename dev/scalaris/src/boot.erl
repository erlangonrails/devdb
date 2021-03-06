%  @copyright 2007-2010 Konrad-Zuse-Zentrum fuer Informationstechnik Berlin

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

%% @author Thorsten Schuett <schuett@zib.de>
%% @doc This is the boot application module. It starts the boot server.
%% @end
%% @version $Id: boot.erl 953 2010-08-03 13:52:39Z kruber@zib.de $
-module(boot).
-author('schuett@zib.de').
-vsn('$Id: boot.erl 953 2010-08-03 13:52:39Z kruber@zib.de $').

-export([start/0, stop/0]).

-spec start() -> ok | {error, Reason::term()}.
start() ->
    %% tracer:start(),
    application:start(boot_cs).

-spec stop() -> ok | {error, Reason::term()}.
stop() ->
    application:stop(boot_cs).

