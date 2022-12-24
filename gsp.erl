-module(gsp).
-export([
    start_gossip/3, main_node_list/1,
    build_topology/5, build_topology/6,
    update_topology/2, update_topology/4, get_node_list/4,
    linear_gossip/5, linear_push_sum/6, 
    update_next/2, update_previous/1,
    grid_gossip/14, grid_push_sum/15,
    full_gossip/3, full_push_sum/4,
    wait_gossip/0
    ]).

main_node_list(NodeNumberList) ->
    receive
        {getnodelist, From} ->
            From ! {updatenodenumberlist, NodeNumberList},
            main_node_list(NodeNumberList);
        {deletefromnodelist, FromNodeNumber} ->
            UpdatedNodeNumberList = lists:delete(FromNodeNumber, NodeNumberList),
            if
                length(UpdatedNodeNumberList) == 0 ->
                    io:format("Shutting Down Main Node~n",[]),
                    {_,Time} = statistics(wall_clock),
                    io:format("The work took ~p milliseconds~n", [Time]),
                    exit(normal);
                true ->
                    ok
            end,
            main_node_list(UpdatedNodeNumberList)
    end.

update_next(Next, Max) ->
    if
        (Next > Max) or (Next == x) ->
            x;
    true ->
        main_node_list ! {getnodelist, self()},
        receive
            {updatenodenumberlist, UpdatedNodeNumberList} ->
                NextActive = lists:member(Next, UpdatedNodeNumberList),
                if
                    NextActive ->
                        Next;
                    not NextActive ->
                        update_next(Next + 1, Max)
                end
        after 5000 ->
            io:format("Main Node List Not Responding~n", []),
            exit(timeout)
        end
    end.

update_previous(Previous) ->
    if
        (Previous < 1) or (Previous == x) ->
            x;
    true ->
        main_node_list ! {getnodelist, self()},
        receive
            {updatenodenumberlist, UpdatedNodeNumberList} ->
                PreviousActive = lists:member(Previous, UpdatedNodeNumberList),
                if
                    PreviousActive ->
                        Previous;
                    not PreviousActive ->
                        update_previous(Previous - 1)
                end
        after 5000 ->
            io:format("Main Node List Not Responding~n", []),
            exit(timeout)
        end
    end.

get_node_list(I, J, N, NodeList) ->
    if 
        I =< N ->
            if
                J =< N ->
                    UpdatedNodeList = lists:append(NodeList, [[I,J]]),
                    get_node_list(I, J + 1, N, UpdatedNodeList);
                true ->
                    get_node_list(I + 1, 1, N, NodeList)
            end;
        true ->
            NodeList
    end.

update_topology(0, NodeList) ->
    NodeList;
update_topology(N, NodeList) ->
    NodeName = "node_"++integer_to_list(N),
    {ok, Node} = dict:find(NodeName, NodeList),
    Node ! {updatenodelist, NodeList},
    update_topology(N - 1, NodeList).

update_topology(I, J, N, NodeList) ->
    if 
        I =< N ->
            if
                J =< N ->
                    NodeName = "node_"++integer_to_list(I)++"_"++integer_to_list(J),
                    {ok, Node} = dict:find(NodeName, NodeList),
                    Node ! {updatenodelist, NodeList},
                    update_topology(I, J + 1, N, NodeList);
                true ->
                    update_topology(I + 1, 1, N, NodeList)
            end;
        true ->
            NodeList
    end.

build_topology(0, NumberOfNodes, NodeList, _, _) ->
    update_topology(NumberOfNodes, NodeList);
build_topology(N, NumberOfNodes, NodeList, Topology, Algorithm) ->
    if
        Topology == line ->
            if 
                Algorithm == gossip ->
                    if 
                        N == NumberOfNodes ->
                            NewNode = spawn(gsp, linear_gossip, [N, N - 1, x, 0, NodeList]);
                        N == 1 ->
                            NewNode = spawn(gsp, linear_gossip, [N, x, N + 1, 0, NodeList]);
                        true ->
                            NewNode = spawn(gsp, linear_gossip, [N, N - 1, N + 1, 0, NodeList])
                    end;
                Algorithm == pushsum ->
                    if 
                        N == NumberOfNodes ->
                            NewNode = spawn(gsp, linear_push_sum, [N, N - 1, x, 0, NodeList, [[N, 1]]]);
                        N == 1 ->
                            NewNode = spawn(gsp, linear_push_sum, [N, x, N + 1, 0, NodeList, [[N, 1]]]);
                        true ->
                            NewNode = spawn(gsp, linear_push_sum, [N, N - 1, N + 1, 0, NodeList, [[N, 1]]])
                    end
            end;
        Topology == full ->
            if 
                Algorithm == gossip ->
                    NewNode = spawn(gsp, full_gossip, [N, 0, NodeList]);
                Algorithm == pushsum ->
                    NewNode = spawn(gsp, full_push_sum, [N, 0, NodeList, [[N, 1]]])
            end
    end,
    NewNodeList = dict:store("node_"++integer_to_list(N), NewNode, NodeList),
    build_topology(N - 1, NumberOfNodes, NewNodeList, Topology, Algorithm).

build_topology(I, J, N, NodeList, Topology, Algorithm) ->
    if 
        I =< N ->
            if
                J =< N ->
                    if 
                        I == 1 ->
                            Top = [x,x];
                        true ->
                            Top = [I - 1, J]
                    end,
                    if 
                        I == N ->
                            Bottom = [x,x];
                        true ->
                            Bottom = [I + 1, J]
                    end,
                    if 
                        J == 1 ->
                            Left = [x,x];
                        true ->
                            Left = [I, J - 1]
                    end,
                    if 
                        J == N ->
                            Right = [x,x];
                        true ->
                            Right = [I, J + 1]
                    end,
                    if
                        (Top == [x,x]) or (Left == [x,x]) ->
                            TopLeft = [x,x];
                        true ->
                            TopLeft = [I - 1, J - 1]
                    end,
                    if
                        (Top == [x,x]) or (Right == [x,x]) ->
                            TopRight = [x,x];
                        true ->
                            TopRight = [I - 1, J + 1]
                    end,
                    if
                        (Bottom == [x,x]) or (Left == [x,x]) ->
                            BottomLeft = [x,x];
                        true ->
                            BottomLeft = [I + 1, J - 1]
                    end,
                    if
                        (Bottom == [x,x]) or (Right == [x,x]) ->
                            BottomRight = [x,x];
                        true ->
                            BottomRight = [I + 1, J + 1]
                    end,
                    if
                        Algorithm == gossip ->
                            NewNode = spawn(gsp, grid_gossip, [I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, 0, NodeList, Topology]);
                        Algorithm == pushsum ->
                            S = (I - 1)*N + J,
                            NewNode = spawn(gsp, grid_push_sum, [I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, 0, NodeList, Topology, [[S,1]]])
                    end,
                    NewNodeList = dict:store("node_"++integer_to_list(I)++"_"++integer_to_list(J), NewNode, NodeList),
                    build_topology(I, J + 1, N, NewNodeList, Topology, Algorithm);
                true ->
                    NewNodeList = NodeList,
                    build_topology(I + 1, 1, N, NewNodeList, Topology, Algorithm)
            end;
        true ->
            update_topology(1, 1, N, NodeList)
    end.

linear_push_sum(NodeNumber, Previous, Next, Count, NodeList, PushSumList) ->
    receive
        {updatenodelist, UpdatedNodeList} ->
            linear_push_sum(NodeNumber, Previous, Next, Count, UpdatedNodeList, PushSumList);
        {updatenext, UpdatedNext} ->
            linear_push_sum(NodeNumber, Previous, UpdatedNext, Count, NodeList, PushSumList);
        {updateprevious, UpdatedPrevious} ->
            linear_push_sum(NodeNumber, UpdatedPrevious, Next, Count, NodeList, PushSumList);
        {get_gossip, PushSumValue} ->
            LastPushSumValue = lists:last(PushSumList),
            NewS = (lists:nth(1,LastPushSumValue)+lists:nth(1,PushSumValue)),
            NewW = (lists:nth(2,LastPushSumValue)+lists:nth(2,PushSumValue)),
            UpdatedPushSumValue = [NewS,NewW],
            UpdatedPushSumList = lists:append(PushSumList, [UpdatedPushSumValue]),
            io:format("Node: ~w (P:~w/N:~w)(s:~w/w:~w). ~w Gossips Received.~n",[NodeNumber, Previous, Next, NewS, NewW, Count + 1]),
            if
                length(UpdatedPushSumList) > 4 ->
                    FinalPushSumList = lists:delete(lists:nth(1,UpdatedPushSumList),UpdatedPushSumList),
                    [S1, W1] = [lists:nth(1, lists:nth(1,FinalPushSumList)), lists:nth(2, lists:nth(1,FinalPushSumList))],
                    [S2, W2] = [lists:nth(1, lists:nth(2,FinalPushSumList)), lists:nth(2, lists:nth(2,FinalPushSumList))],
                    [S3, W3] = [lists:nth(1, lists:nth(3,FinalPushSumList)), lists:nth(2, lists:nth(3,FinalPushSumList))],
                    [S4, W4] = [lists:nth(1, lists:nth(4,FinalPushSumList)), lists:nth(2, lists:nth(4,FinalPushSumList))],
                    Diff1 = abs((S1/W1)-(S2/W2)),
                    Diff2 = abs((S2/W2)-(S3/W3)),
                    Diff3 = abs((S3/W3)-(S4/W4)),
                    MaxDiff = math:pow(10,-10),
                    if
                        (Diff1 < MaxDiff) and (Diff2 < MaxDiff) and (Diff3 < MaxDiff) ->        
                            if
                                Next =/= x ->
                                    NextNodeName = "node_"++integer_to_list(Next),
                                    {ok, NextNode} = dict:find(NextNodeName, NodeList),
                                    NextNode ! {updateprevious, Previous};
                                true ->
                                    ok
                            end,
                            if
                                Previous =/= x ->
                                    PreviousNodeName = "node_"++integer_to_list(Previous),
                                    {ok, PreviousNode} = dict:find(PreviousNodeName, NodeList),
                                    PreviousNode ! {updatenext, Next};
                                true ->
                                    ok
                            end,
                            main_node_list ! {deletefromnodelist, NodeNumber},
                            io:format("Node: ~w (P:~w/N:~w)(s:~w/w:~w). Push Sum Converged after ~w Gossips. Shutting Down.~n",[NodeNumber, Previous, Next, NewS, NewW, Count + 1]),
                            exit(normal);
                        true ->
                            ok
                    end;
                true ->
                    FinalPushSumList = UpdatedPushSumList
            end,
            linear_push_sum(NodeNumber, Previous, Next, Count + 1, NodeList, FinalPushSumList)
    after 500 ->
        if 
            Count > 0 ->
                ok;
            true ->
                linear_push_sum(NodeNumber, Previous, Next, Count, NodeList, PushSumList)
        end,
        main_node_list ! {getnodelist, self()},
        receive
            {updatenodenumberlist, UpdatedNodeNumberList} ->
                AllActiveNodeList = UpdatedNodeNumberList
        after 5000 ->
            io:format("Main Node List Not Responding~n", []),
            AllActiveNodeList = [],
            exit(timeout)
        end,
        PreviousActive = lists:member(Previous, AllActiveNodeList),
        NextActive = lists:member(Next, AllActiveNodeList),
        if
            (Previous == x) or not PreviousActive ->
                UpdatedPrevious = x,
                PreviousNodeList = [];
            true ->
                UpdatedPrevious = Previous,
                PreviousNodeList = [Previous]
        end,
        if
            (Next == x) or not NextActive ->
                UpdatedNext = x,
                NextNodeList = [];
            true ->
                UpdatedNext = Next,
                NextNodeList = [Next]
        end,
        NeighborNodeList = lists:append(PreviousNodeList, NextNodeList),
        [S, W] = lists:last(PushSumList),
        UpdatedPushSumList = lists:append(lists:delete([S,W],PushSumList),[[S/2,W/2]]),
        if
            length(NeighborNodeList) == 0 ->
                main_node_list ! {deletefromnodelist, NodeNumber},
                io:format("No Neighbors Left. ~w Gossips Heard. Shutting Down. Node ~w (P:~w/N:~w)(s:~w/w:~w)~n",[Count, NodeNumber, UpdatedPrevious, UpdatedNext, S, W]),
                exit(normal);
            true ->
                SendNodeNumber = lists:nth(rand:uniform(length(NeighborNodeList)), NeighborNodeList),
                SendNodeName = "node_"++integer_to_list(SendNodeNumber),
                {ok, SendNode} = dict:find(SendNodeName, NodeList),
                SendNode ! {get_gossip, [S/2, W/2]},
                io:format("Node: ~w (P:~w/N:~w)(s:~w/w:~w). Forwarding to ~w.~n",[NodeNumber, UpdatedPrevious, UpdatedNext, S/2, W/2, SendNodeNumber]),
                linear_push_sum(NodeNumber, Previous, Next, Count, NodeList, UpdatedPushSumList)
        end
    end.
    
linear_gossip(NodeNumber, Previous, Next, Count, NodeList) ->
    receive
        {updatenodelist, UpdatedNodeList} ->
            linear_gossip(NodeNumber, Previous, Next, Count, UpdatedNodeList);
        {updatenext, UpdatedNext} ->
            linear_gossip(NodeNumber, Previous, UpdatedNext, Count, NodeList);
        {updateprevious, UpdatedPrevious} ->
            linear_gossip(NodeNumber, UpdatedPrevious, Next, Count, NodeList);
        get_gossip ->
            UpdatedNext = update_next(Next, length(dict:to_list(NodeList))),
            UpdatedPrevious = update_previous(Previous),
            if
                Count >= 4 ->
                    if
                        UpdatedNext =/= x ->
                            NextNodeName = "node_"++integer_to_list(UpdatedNext),
                            {ok, NextNode} = dict:find(NextNodeName, NodeList),
                            NextNode ! {updateprevious, Previous};
                        true ->
                            ok
                    end,
                    if
                        UpdatedPrevious =/= x ->
                            PreviousNodeName = "node_"++integer_to_list(UpdatedPrevious),
                            {ok, PreviousNode} = dict:find(PreviousNodeName, NodeList),
                            PreviousNode ! {updatenext, UpdatedNext};
                        true ->
                            ok
                    end,
                    main_node_list ! {deletefromnodelist, NodeNumber},
                    io:format("Node: ~w (P:~w/N:~w). 5 Gossips Heard. Shutting Down.~n",[NodeNumber, UpdatedPrevious, UpdatedNext]),
                    exit(normal);
                true ->
                    io:format("Node: ~w (P:~w/N:~w). ~w Gossips Received.~n",[NodeNumber, UpdatedPrevious, UpdatedNext, Count + 1]),
                    linear_gossip(NodeNumber, UpdatedPrevious, UpdatedNext, Count + 1, NodeList)
            end
    after 500 ->
        if 
            Count > 0 ->
                ok;
            true ->
                linear_gossip(NodeNumber, Previous, Next, Count, NodeList)
        end,
        main_node_list ! {getnodelist, self()},
        receive
            {updatenodenumberlist, UpdatedNodeNumberList} ->
                AllActiveNodeList = UpdatedNodeNumberList
        after 5000 ->
            io:format("Main Node List Not Responding~n", []),
            AllActiveNodeList = [],
            exit(timeout)
        end,
        PreviousActive = lists:member(Previous, AllActiveNodeList),
        NextActive = lists:member(Next, AllActiveNodeList),
        if
            (Previous == x) or not PreviousActive ->
                UpdatedPrevious = update_previous(Previous),
                if
                    UpdatedPrevious == x ->
                        PreviousNodeList = [];
                    true ->
                        PreviousNodeList = [UpdatedPrevious]
                end;
            true ->
                UpdatedPrevious = Previous,
                PreviousNodeList = [UpdatedPrevious]
        end,
        if
            (Next == x) or not NextActive ->
                UpdatedNext = update_next(Next, length(dict:to_list(NodeList))),
                if
                    UpdatedNext == x ->
                        NextNodeList = [];
                    true ->
                        NextNodeList = [UpdatedNext]
                end;
            true ->
                UpdatedNext = Next,
                NextNodeList = [UpdatedNext]
        end,
        NeighborNodeList = lists:append(PreviousNodeList, NextNodeList),        
        if
            length(NeighborNodeList) == 0 ->
                main_node_list ! {deletefromnodelist, NodeNumber},
                io:format("No Neighbors Left. ~w Gossips Heard. Shutting Down. Node ~w (P:~w/N:~w)~n",[Count, NodeNumber, UpdatedPrevious, UpdatedNext]),
                exit(normal);
            true ->
                SendNodeNumber = lists:nth(rand:uniform(length(NeighborNodeList)), NeighborNodeList),
                SendNodeName = "node_"++integer_to_list(SendNodeNumber),
                {ok, SendNode} = dict:find(SendNodeName, NodeList),
                SendNode ! get_gossip,
                io:format("Node: ~w (P:~w/N:~w). Forwarding to ~w.~n",[NodeNumber, UpdatedPrevious, UpdatedNext, SendNodeNumber]),
                linear_gossip(NodeNumber, Previous, Next, Count, NodeList)
        end
    end.

grid_push_sum(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type, PushSumList) ->
    receive
        {updatenodelist, UpdatedNodeList} ->
            grid_push_sum(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, UpdatedNodeList, Type, PushSumList);
        {updatetop, UpdatedTop} ->
            grid_push_sum(I, J, N, UpdatedTop, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type, PushSumList);
        {updatebottom, UpdatedBottom} ->
            grid_push_sum(I, J, N, Top, UpdatedBottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type, PushSumList);
        {updateleft, UpdatedLeft} ->
            grid_push_sum(I, J, N, Top, Bottom, UpdatedLeft, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type, PushSumList);
        {updateright, UpdatedRight} ->
            grid_push_sum(I, J, N, Top, Bottom, Left, UpdatedRight, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type, PushSumList);
        {updatetopleft, UpdatedTopLeft} ->
            grid_push_sum(I, J, N, Top, Bottom, Left, Right, UpdatedTopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type, PushSumList);
        {updatetopright, UpdatedTopRight} ->
            grid_push_sum(I, J, N, Top, Bottom, Left, Right, TopLeft, UpdatedTopRight, BottomLeft, BottomRight, Count, NodeList, Type, PushSumList);
        {updatebottomleft, UpdatedBottomLeft} ->
            grid_push_sum(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, UpdatedBottomLeft, BottomRight, Count, NodeList, Type, PushSumList);
        {updatebottomright, UpdatedBottomRight} ->
            grid_push_sum(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, UpdatedBottomRight, Count, NodeList, Type, PushSumList);
        {get_gossip, PushSumValue} ->
            LastPushSumValue = lists:last(PushSumList),
            NewS = (lists:nth(1,LastPushSumValue)+lists:nth(1,PushSumValue)),
            NewW = (lists:nth(2,LastPushSumValue)+lists:nth(2,PushSumValue)),
            UpdatedPushSumValue = [NewS,NewW],
            UpdatedPushSumList = lists:append(PushSumList, [UpdatedPushSumValue]),
            io:format("Node: ~w|~w (U:~w/D:~w/L:~w/R:~w/TL:~w/TR:~w/BL:~w,BR:~w)(s:~w/w:~w). ~w Gossips Received.~n",[I, J, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, NewS, NewW, Count + 1]),
            if
                length(UpdatedPushSumList) > 4 ->
                    FinalPushSumList = lists:delete(lists:nth(1,UpdatedPushSumList),UpdatedPushSumList),
                    [S1, W1] = [lists:nth(1, lists:nth(1,FinalPushSumList)), lists:nth(2, lists:nth(1,FinalPushSumList))],
                    [S2, W2] = [lists:nth(1, lists:nth(2,FinalPushSumList)), lists:nth(2, lists:nth(2,FinalPushSumList))],
                    [S3, W3] = [lists:nth(1, lists:nth(3,FinalPushSumList)), lists:nth(2, lists:nth(3,FinalPushSumList))],
                    [S4, W4] = [lists:nth(1, lists:nth(4,FinalPushSumList)), lists:nth(2, lists:nth(4,FinalPushSumList))],
                    Diff1 = abs((S1/W1)-(S2/W2)),
                    Diff2 = abs((S2/W2)-(S3/W3)),
                    Diff3 = abs((S3/W3)-(S4/W4)),
                    MaxDiff = math:pow(10,-10),
                    if
                        (Diff1 < MaxDiff) and (Diff2 < MaxDiff) and (Diff3 < MaxDiff) ->
                            if
                                Top =/= [x,x] ->
                                    TopNodeName = "node_"++integer_to_list(lists:nth(1,Top))++"_"++integer_to_list(lists:nth(2,Top)),
                                    {ok, TopNode} = dict:find(TopNodeName, NodeList),
                                    TopNode ! {updatebottom, Bottom};
                                true ->
                                    ok
                            end,
                            if
                                Bottom =/= [x,x] ->
                                    BottomNodeName = "node_"++integer_to_list(lists:nth(1,Bottom))++"_"++integer_to_list(lists:nth(2,Bottom)),
                                    {ok, BottomNode} = dict:find(BottomNodeName, NodeList),
                                    BottomNode ! {updatetop, Top};
                                true ->
                                    ok
                            end,
                            if
                                Left =/= [x,x] ->
                                    LeftNodeName = "node_"++integer_to_list(lists:nth(1,Left))++"_"++integer_to_list(lists:nth(2,Left)),
                                    {ok, LeftNode} = dict:find(LeftNodeName, NodeList),
                                    LeftNode ! {updateright, Right};
                                true ->
                                    ok
                            end,
                            if
                                Right =/= [x,x] ->
                                    RightNodeName = "node_"++integer_to_list(lists:nth(1,Right))++"_"++integer_to_list(lists:nth(2,Right)),
                                    {ok, RightNode} = dict:find(RightNodeName, NodeList),
                                    RightNode ! {updateleft, Left};
                                true ->
                                    ok
                            end,
                            if
                                TopLeft =/= [x,x] ->
                                    TopLeftNodeName = "node_"++integer_to_list(lists:nth(1,TopLeft))++"_"++integer_to_list(lists:nth(2,TopLeft)),
                                    {ok, TopLeftNode} = dict:find(TopLeftNodeName, NodeList),
                                    TopLeftNode ! {updatebottomright, BottomRight};
                                true ->
                                    ok
                            end,
                            if
                                TopRight =/= [x,x] ->
                                    TopRightNodeName = "node_"++integer_to_list(lists:nth(1,TopRight))++"_"++integer_to_list(lists:nth(2,TopRight)),
                                    {ok, TopRightNode} = dict:find(TopRightNodeName, NodeList),
                                    TopRightNode ! {updatebottomleft, BottomLeft};
                                true ->
                                    ok
                            end,
                            if
                                BottomLeft =/= [x,x] ->
                                    BottomLeftNodeName = "node_"++integer_to_list(lists:nth(1,BottomLeft))++"_"++integer_to_list(lists:nth(2,BottomLeft)),
                                    {ok, BottomLeftNode} = dict:find(BottomLeftNodeName, NodeList),
                                    BottomLeftNode ! {updatetopright, TopRight};
                                true ->
                                    ok
                            end,
                            if
                                BottomRight =/= [x,x] ->
                                    BottomRightNodeName = "node_"++integer_to_list(lists:nth(1,BottomRight))++"_"++integer_to_list(lists:nth(2,BottomRight)),
                                    {ok, BottomRightNode} = dict:find(BottomRightNodeName, NodeList),
                                    BottomRightNode ! {updatetopleft, TopLeft};
                                true ->
                                    ok
                            end,
                            main_node_list ! {deletefromnodelist, [I,J]},
                            io:format("Node: ~w|~w (U:~w/D:~w/L:~w/R:~w/TL:~w/TR:~w/BL:~w,BR:~w)(s:~w/w:~w). Push Sum Converged after ~w Gossips. Shutting Down.~nn",[I, J, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, NewS, NewW, Count + 1]),
                            exit(normal);
                        true ->
                            ok
                    end;
                true ->
                    FinalPushSumList = UpdatedPushSumList
            end,
            grid_push_sum(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count + 1, NodeList, Type, FinalPushSumList)
    after 500 ->
        if 
            Count > 0 ->
                ok;
            true ->
                grid_push_sum(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type, PushSumList)
        end,
        main_node_list ! {getnodelist, self()},
        receive
            {updatenodenumberlist, UpdatedNodeNumberList} ->
                AllActiveNodeList = UpdatedNodeNumberList
        after 5000 ->
            io:format("Main Node List Not Responding~n", []),
            AllActiveNodeList = [],
            exit(timeout)
        end,
        TopActive = lists:member(Top, AllActiveNodeList),
        BottomActive = lists:member(Bottom, AllActiveNodeList),
        LeftActive = lists:member(Left, AllActiveNodeList),
        RightActive = lists:member(Right, AllActiveNodeList),
        TopLeftActive = lists:member(TopLeft, AllActiveNodeList),
        TopRightActive = lists:member(TopRight, AllActiveNodeList),
        BottomLeftActive = lists:member(BottomLeft, AllActiveNodeList),
        BottomRightActive = lists:member(BottomRight, AllActiveNodeList),
        if
            (Top == [x,x]) or not TopActive ->
                UpdatedTop = [x,x],
                TopNodeList = [];
            true ->
                UpdatedTop = Top,
                TopNodeList = [Top]
        end,
        if
            (Bottom == [x,x]) or not BottomActive ->
                UpdatedBottom = [x,x],
                BottomNodeList = [];
            true ->
                UpdatedBottom = Bottom,
                BottomNodeList = [Bottom]
        end,
        if
            (Left == [x,x]) or not LeftActive ->
                UpdatedLeft = [x,x],
                LeftNodeList = [];
            true ->
                UpdatedLeft = Left,
                LeftNodeList = [Left]
        end,
        if
            (Right == [x,x]) or not RightActive ->
                UpdatedRight = [x,x],
                RightNodeList = [];
            true ->
                UpdatedRight = Right,
                RightNodeList = [Right]
        end,
        if
            (TopLeft == [x,x]) or not TopLeftActive ->
                UpdatedTopLeft = [x,x],
                TopLeftNodeList = [];
            true ->
                UpdatedTopLeft = TopLeft,
                TopLeftNodeList = [TopLeft]
        end,
        if
            (TopRight == [x,x]) or not TopRightActive ->
                UpdatedTopRight = [x,x],
                TopRightNodeList = [];
            true ->
                UpdatedTopRight = TopRight,
                TopRightNodeList = [TopRight]
        end,
        if
            (BottomLeft == [x,x]) or not BottomLeftActive ->
                UpdatedBottomLeft = [x,x],
                BottomLeftNodeList = [];
            true ->
                UpdatedBottomLeft = BottomLeft,
                BottomLeftNodeList = [BottomLeft]
        end,
        if
            (BottomRight == [x,x]) or not BottomRightActive ->
                UpdatedBottomRight = [x,x],
                BottomRightNodeList = [];
            true ->
                UpdatedBottomRight = BottomRight,
                BottomRightNodeList = [BottomRight]
        end,
        if
            Type == imperfect ->
                ActiveNodeMinusTopList = lists:delete(UpdatedTop, AllActiveNodeList),
                ActiveNodeMinusBottomList = lists:delete(UpdatedBottom, ActiveNodeMinusTopList),
                ActiveNodeMinusLeftList = lists:delete(UpdatedLeft, ActiveNodeMinusBottomList),
                ActiveNodeMinusRightList = lists:delete(UpdatedRight, ActiveNodeMinusLeftList),
                ActiveNodeMinusTopLeftList = lists:delete(UpdatedTopLeft, ActiveNodeMinusRightList),
                ActiveNodeMinusTopRightList = lists:delete(UpdatedTopRight, ActiveNodeMinusTopLeftList),
                ActiveNodeMinusBottomLeftList = lists:delete(UpdatedBottomLeft, ActiveNodeMinusTopRightList),
                ActiveNodeMinusBottomRightList = lists:delete(UpdatedBottomRight, ActiveNodeMinusBottomLeftList),
                ActiveNodeMinusSelfList = lists:delete([I,J], ActiveNodeMinusBottomRightList),
                RandomNode = lists:nth(rand:uniform(length(ActiveNodeMinusSelfList)), ActiveNodeMinusSelfList),
                RandomNodeList = [RandomNode];
            Type == grid ->
                RandomNodeList = []
        end,
        NeighborNodeList = lists:append([TopNodeList, BottomNodeList, LeftNodeList, RightNodeList, TopLeftNodeList, TopRightNodeList, BottomLeftNodeList, BottomRightNodeList, RandomNodeList]),
        [S, W] = lists:last(PushSumList),
        UpdatedPushSumList = lists:append(lists:delete([S,W],PushSumList),[[S/2,W/2]]),
        if
            length(NeighborNodeList) ==0 ->
                main_node_list ! {deletefromnodelist, [I,J]},
                io:format("No Neighbors Left. ~w Gossips Heard. Shutting Down Node ~w|~w (U:~w/D:~w/L:~w/R:~w/TL:~w/TR:~w/BL:~w,BR:~w)(s:~w/w:~w)~n",[Count, I, J, UpdatedTop, UpdatedBottom, UpdatedLeft, UpdatedRight, UpdatedTopLeft, UpdatedTopRight, UpdatedBottomLeft, UpdatedBottomRight, S, W]),
                exit(normal);
            true ->
                SendNodeNumber = lists:nth(rand:uniform(length(NeighborNodeList)), NeighborNodeList),
                SendNodeName = "node_"++integer_to_list(lists:nth(1,SendNodeNumber))++"_"++integer_to_list(lists:nth(2,SendNodeNumber)),
                {ok, SendNode} = dict:find(SendNodeName, NodeList),
                SendNode ! {get_gossip, [S/2, W/2]},
                io:format("Node: ~w|~w (U:~w/D:~w/L:~w/R:~w/TL:~w/TR:~w/BL:~w,BR:~w)(s:~w/w:~w). Forwarding to ~w.~n",[I, J, UpdatedTop, UpdatedBottom, UpdatedLeft, UpdatedRight, UpdatedTopLeft, UpdatedTopRight, UpdatedBottomLeft, UpdatedBottomRight, S/2, W/2, SendNodeNumber]),
                grid_push_sum(I, J, N, UpdatedTop, UpdatedBottom, UpdatedLeft, UpdatedRight, UpdatedTopLeft, UpdatedTopRight, UpdatedBottomLeft, UpdatedBottomRight, Count, NodeList, Type, UpdatedPushSumList)
        end
    end.
    
    

grid_gossip(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type) ->
    receive
        {updatenodelist, UpdatedNodeList} ->
            grid_gossip(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, UpdatedNodeList, Type);
        {updatetop, UpdatedTop} ->
            grid_gossip(I, J, N, UpdatedTop, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type);
        {updatebottom, UpdatedBottom} ->
            grid_gossip(I, J, N, Top, UpdatedBottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type);
        {updateleft, UpdatedLeft} ->
            grid_gossip(I, J, N, Top, Bottom, UpdatedLeft, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type);
        {updateright, UpdatedRight} ->
            grid_gossip(I, J, N, Top, Bottom, Left, UpdatedRight, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type);
        {updatetopleft, UpdatedTopLeft} ->
            grid_gossip(I, J, N, Top, Bottom, Left, Right, UpdatedTopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type);
        {updatetopright, UpdatedTopRight} ->
            grid_gossip(I, J, N, Top, Bottom, Left, Right, TopLeft, UpdatedTopRight, BottomLeft, BottomRight, Count, NodeList, Type);
        {updatebottomleft, UpdatedBottomLeft} ->
            grid_gossip(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, UpdatedBottomLeft, BottomRight, Count, NodeList, Type);
        {updatebottomright, UpdatedBottomRight} ->
            grid_gossip(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, UpdatedBottomRight, Count, NodeList, Type);
        get_gossip ->
            if
                Count >= 4 ->
                    if
                        Top =/= [x,x] ->
                            TopNodeName = "node_"++integer_to_list(lists:nth(1,Top))++"_"++integer_to_list(lists:nth(2,Top)),
                            {ok, TopNode} = dict:find(TopNodeName, NodeList),
                            TopNode ! {updatebottom, Bottom};
                        true ->
                            ok
                    end,
                    if
                        Bottom =/= [x,x] ->
                            BottomNodeName = "node_"++integer_to_list(lists:nth(1,Bottom))++"_"++integer_to_list(lists:nth(2,Bottom)),
                            {ok, BottomNode} = dict:find(BottomNodeName, NodeList),
                            BottomNode ! {updatetop, Top};
                        true ->
                            ok
                    end,
                    if
                        Left =/= [x,x] ->
                            LeftNodeName = "node_"++integer_to_list(lists:nth(1,Left))++"_"++integer_to_list(lists:nth(2,Left)),
                            {ok, LeftNode} = dict:find(LeftNodeName, NodeList),
                            LeftNode ! {updateright, Right};
                        true ->
                            ok
                    end,
                    if
                        Right =/= [x,x] ->
                            RightNodeName = "node_"++integer_to_list(lists:nth(1,Right))++"_"++integer_to_list(lists:nth(2,Right)),
                            {ok, RightNode} = dict:find(RightNodeName, NodeList),
                            RightNode ! {updateleft, Left};
                        true ->
                            ok
                    end,
                    if
                        TopLeft =/= [x,x] ->
                            TopLeftNodeName = "node_"++integer_to_list(lists:nth(1,TopLeft))++"_"++integer_to_list(lists:nth(2,TopLeft)),
                            {ok, TopLeftNode} = dict:find(TopLeftNodeName, NodeList),
                            TopLeftNode ! {updatebottomright, BottomRight};
                        true ->
                            ok
                    end,
                    if
                        TopRight =/= [x,x] ->
                            TopRightNodeName = "node_"++integer_to_list(lists:nth(1,TopRight))++"_"++integer_to_list(lists:nth(2,TopRight)),
                            {ok, TopRightNode} = dict:find(TopRightNodeName, NodeList),
                            TopRightNode ! {updatebottomleft, BottomLeft};
                        true ->
                            ok
                    end,
                    if
                        BottomLeft =/= [x,x] ->
                            BottomLeftNodeName = "node_"++integer_to_list(lists:nth(1,BottomLeft))++"_"++integer_to_list(lists:nth(2,BottomLeft)),
                            {ok, BottomLeftNode} = dict:find(BottomLeftNodeName, NodeList),
                            BottomLeftNode ! {updatetopright, TopRight};
                        true ->
                            ok
                    end,
                    if
                        BottomRight =/= [x,x] ->
                            BottomRightNodeName = "node_"++integer_to_list(lists:nth(1,BottomRight))++"_"++integer_to_list(lists:nth(2,BottomRight)),
                            {ok, BottomRightNode} = dict:find(BottomRightNodeName, NodeList),
                            BottomRightNode ! {updatetopleft, TopLeft};
                        true ->
                            ok
                    end,
                    main_node_list ! {deletefromnodelist, [I,J]},
                    io:format("Node: ~w|~w (U:~w/D:~w/L:~w/R:~w/TL:~w/TR:~w/BL:~w,BR:~w). 5 Gossips Heard. Shutting Down.~n",[I, J, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight]),
                    exit(normal);
                true ->
                    io:format("Node: ~w|~w (U:~w/D:~w/L:~w/R:~w/TL:~w/TR:~w/BL:~w,BR:~w). ~w Gossips Received.~n",[I, J, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count + 1]),
                    grid_gossip(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count + 1, NodeList, Type)
            end
    after 500 ->
        if 
            Count > 0 ->
                ok;
            true ->
                grid_gossip(I, J, N, Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight, Count, NodeList, Type)
        end,
        main_node_list ! {getnodelist, self()},
        receive
            {updatenodenumberlist, UpdatedNodeNumberList} ->
                AllActiveNodeList = UpdatedNodeNumberList
        after 5000 ->
            io:format("Main Node List Not Responding~n", []),
            AllActiveNodeList = [],
            exit(timeout)
        end,
        TopActive = lists:member(Top, AllActiveNodeList),
        BottomActive = lists:member(Bottom, AllActiveNodeList),
        LeftActive = lists:member(Left, AllActiveNodeList),
        RightActive = lists:member(Right, AllActiveNodeList),
        TopLeftActive = lists:member(TopLeft, AllActiveNodeList),
        TopRightActive = lists:member(TopRight, AllActiveNodeList),
        BottomLeftActive = lists:member(BottomLeft, AllActiveNodeList),
        BottomRightActive = lists:member(BottomRight, AllActiveNodeList),
        if
            (Top == [x,x]) or not TopActive ->
                UpdatedTop = [x,x],
                TopNodeList = [];
            true ->
                UpdatedTop = Top,
                TopNodeList = [Top]
        end,
        if
            (Bottom == [x,x]) or not BottomActive ->
                UpdatedBottom = [x,x],
                BottomNodeList = [];
            true ->
                UpdatedBottom = Bottom,
                BottomNodeList = [Bottom]
        end,
        if
            (Left == [x,x]) or not LeftActive ->
                UpdatedLeft = [x,x],
                LeftNodeList = [];
            true ->
                UpdatedLeft = Left,
                LeftNodeList = [Left]
        end,
        if
            (Right == [x,x]) or not RightActive ->
                UpdatedRight = [x,x],
                RightNodeList = [];
            true ->
                UpdatedRight = Right,
                RightNodeList = [Right]
        end,
        if
            (TopLeft == [x,x]) or not TopLeftActive ->
                UpdatedTopLeft = [x,x],
                TopLeftNodeList = [];
            true ->
                UpdatedTopLeft = TopLeft,
                TopLeftNodeList = [TopLeft]
        end,
        if
            (TopRight == [x,x]) or not TopRightActive ->
                UpdatedTopRight = [x,x],
                TopRightNodeList = [];
            true ->
                UpdatedTopRight = TopRight,
                TopRightNodeList = [TopRight]
        end,
        if
            (BottomLeft == [x,x]) or not BottomLeftActive ->
                UpdatedBottomLeft = [x,x],
                BottomLeftNodeList = [];
            true ->
                UpdatedBottomLeft = BottomLeft,
                BottomLeftNodeList = [BottomLeft]
        end,
        if
            (BottomRight == [x,x]) or not BottomRightActive ->
                UpdatedBottomRight = [x,x],
                BottomRightNodeList = [];
            true ->
                UpdatedBottomRight = BottomRight,
                BottomRightNodeList = [BottomRight]
        end,
        if
            Type == imperfect ->
                ActiveNodeMinusTopList = lists:delete(UpdatedTop, AllActiveNodeList),
                ActiveNodeMinusBottomList = lists:delete(UpdatedBottom, ActiveNodeMinusTopList),
                ActiveNodeMinusLeftList = lists:delete(UpdatedLeft, ActiveNodeMinusBottomList),
                ActiveNodeMinusRightList = lists:delete(UpdatedRight, ActiveNodeMinusLeftList),
                ActiveNodeMinusTopLeftList = lists:delete(UpdatedTopLeft, ActiveNodeMinusRightList),
                ActiveNodeMinusTopRightList = lists:delete(UpdatedTopRight, ActiveNodeMinusTopLeftList),
                ActiveNodeMinusBottomLeftList = lists:delete(UpdatedBottomLeft, ActiveNodeMinusTopRightList),
                ActiveNodeMinusBottomRightList = lists:delete(UpdatedBottomRight, ActiveNodeMinusBottomLeftList),
                ActiveNodeMinusSelfList = lists:delete([I,J], ActiveNodeMinusBottomRightList),
                RandomNode = lists:nth(rand:uniform(length(ActiveNodeMinusSelfList)), ActiveNodeMinusSelfList),
                RandomNodeList = [RandomNode];
            Type == grid ->
                RandomNodeList = []
        end,
        NeighborNodeList = lists:append([TopNodeList, BottomNodeList, LeftNodeList, RightNodeList, TopLeftNodeList, TopRightNodeList, BottomLeftNodeList, BottomRightNodeList, RandomNodeList]),
        if
            length(NeighborNodeList) ==0 ->
                main_node_list ! {deletefromnodelist, [I,J]},
                io:format("No Neighbors Left. ~w Gossips Heard. Shutting Down Node ~w|~w (U:~w/D:~w/L:~w/R:~w/TL:~w/TR:~w/BL:~w,BR:~w)~n",[Count, I, J, UpdatedTop, UpdatedBottom, UpdatedLeft, UpdatedRight, UpdatedTopLeft, UpdatedTopRight, UpdatedBottomLeft, UpdatedBottomRight]),
                exit(normal);
            true ->
                SendNodeNumber = lists:nth(rand:uniform(length(NeighborNodeList)), NeighborNodeList),
                SendNodeName = "node_"++integer_to_list(lists:nth(1,SendNodeNumber))++"_"++integer_to_list(lists:nth(2,SendNodeNumber)),
                {ok, SendNode} = dict:find(SendNodeName, NodeList),
                SendNode ! get_gossip,
                io:format("Node: ~w|~w (U:~w/D:~w/L:~w/R:~w/TL:~w/TR:~w/BL:~w,BR:~w). Forwarding to ~w.~n",[I, J, UpdatedTop, UpdatedBottom, UpdatedLeft, UpdatedRight, UpdatedTopLeft, UpdatedTopRight, UpdatedBottomLeft, UpdatedBottomRight, SendNodeNumber]),
                grid_gossip(I, J, N, UpdatedTop, UpdatedBottom, UpdatedLeft, UpdatedRight, UpdatedTopLeft, UpdatedTopRight, UpdatedBottomLeft, UpdatedBottomRight, Count, NodeList, Type)
        end
    end.

full_push_sum(NodeNumber, Count, NodeList, PushSumList) ->
    receive
        {updatenodelist, UpdatedNodeList} ->
            full_push_sum(NodeNumber, Count, UpdatedNodeList, PushSumList);
        {get_gossip, PushSumValue} ->
            LastPushSumValue = lists:last(PushSumList),
            NewS = (lists:nth(1,LastPushSumValue)+lists:nth(1,PushSumValue)),
            NewW = (lists:nth(2,LastPushSumValue)+lists:nth(2,PushSumValue)),
            UpdatedPushSumValue = [NewS,NewW],
            UpdatedPushSumList = lists:append(PushSumList, [UpdatedPushSumValue]),
            io:format("Node: ~w (s:~w/w:~w). ~w Gossips Received.~n",[NodeNumber, NewS, NewW, Count + 1]),
            if
                length(UpdatedPushSumList) > 4 ->
                    FinalPushSumList = lists:delete(lists:nth(1,UpdatedPushSumList),UpdatedPushSumList),
                    [S1, W1] = [lists:nth(1, lists:nth(1,FinalPushSumList)), lists:nth(2, lists:nth(1,FinalPushSumList))],
                    [S2, W2] = [lists:nth(1, lists:nth(2,FinalPushSumList)), lists:nth(2, lists:nth(2,FinalPushSumList))],
                    [S3, W3] = [lists:nth(1, lists:nth(3,FinalPushSumList)), lists:nth(2, lists:nth(3,FinalPushSumList))],
                    [S4, W4] = [lists:nth(1, lists:nth(4,FinalPushSumList)), lists:nth(2, lists:nth(4,FinalPushSumList))],
                    Diff1 = abs((S1/W1)-(S2/W2)),
                    Diff2 = abs((S2/W2)-(S3/W3)),
                    Diff3 = abs((S3/W3)-(S4/W4)),
                    MaxDiff = math:pow(10,-10),
                    if
                        (Diff1 < MaxDiff) and (Diff2 < MaxDiff) and (Diff3 < MaxDiff) ->  
                            main_node_list ! {deletefromnodelist, NodeNumber},
                            io:format("Node: ~w (s:~w/w:~w). Push Sum Converged after ~w Gossips. Shutting Down.~n",[NodeNumber, NewS, NewW, Count + 1]),
                            exit(normal);
                        true ->
                            ok
                    end;
                true ->
                    FinalPushSumList = UpdatedPushSumList
            end,
            full_push_sum(NodeNumber, Count + 1, NodeList, FinalPushSumList)
    after 500 ->
        if 
            Count > 0 ->
                ok;
            true ->
                full_push_sum(NodeNumber, Count, NodeList, PushSumList)
        end,
        main_node_list ! {getnodelist, self()},
        receive
            {updatenodenumberlist, UpdatedNodeNumberList} ->
                NeighborNodeList = lists:delete(NodeNumber, UpdatedNodeNumberList)
        after 5000 ->
            io:format("Main Node List Not Responding~n", []),
            NeighborNodeList = [],
            exit(timeout)
        end,
        [S, W] = lists:last(PushSumList),
        UpdatedPushSumList = lists:append(lists:delete([S,W],PushSumList),[[S/2,W/2]]),
        if
            length(NeighborNodeList) == 0 ->
                main_node_list ! {deletefromnodelist, NodeNumber},
                io:format("No Neighbors Left. ~w Gossips Heard. Shutting Down Node ~w(s:~w/n:~w)~n",[Count, NodeNumber, S, W]),
                exit(normal);
            true ->
                SendNodeNumber = lists:nth(rand:uniform(length(NeighborNodeList)), NeighborNodeList),
                SendNodeName = "node_"++integer_to_list(SendNodeNumber),
                {ok, SendNode} = dict:find(SendNodeName, NodeList),
                SendNode ! {get_gossip, [S/2, W/2]},
                io:format("Node: ~w. Forwarding to ~w.~n",[NodeNumber, SendNodeNumber]),
                full_push_sum(NodeNumber, Count, NodeList, UpdatedPushSumList)
        end
    end.
    
full_gossip(NodeNumber, Count, NodeList) ->
    receive
        {updatenodelist, UpdatedNodeList} ->
            full_gossip(NodeNumber, Count, UpdatedNodeList);
        get_gossip ->
            if
                Count >= 4 ->
                    main_node_list ! {deletefromnodelist, NodeNumber}, 
                    io:format("Node: ~w. 5 Gossips Heard. Shutting Down.~n",[NodeNumber]),
                    exit(normal);
                true ->
                    io:format("Node: ~w. ~w Gossips Received.~n",[NodeNumber, Count + 1]),
                    full_gossip(NodeNumber, Count + 1, NodeList)
            end
    after 500 ->
        if 
            Count > 0 ->
                ok;
            true ->
                full_gossip(NodeNumber, Count, NodeList)
        end,
        main_node_list ! {getnodelist, self()},
        receive
            {updatenodenumberlist, UpdatedNodeNumberList} ->
                NeighborNodeList = lists:delete(NodeNumber, UpdatedNodeNumberList)
        after 5000 ->
            io:format("Main Node List Not Responding~n", []),
            NeighborNodeList = [],
            exit(timeout)
        end,
        if
            length(NeighborNodeList) == 0 ->
                main_node_list ! {deletefromnodelist, NodeNumber},
                io:format("No Neighbors Left. ~w Gossips Heard. Shutting Down Node ~w~n",[Count, NodeNumber]),
                exit(normal);
            true ->
                SendNodeNumber = lists:nth(rand:uniform(length(NeighborNodeList)), NeighborNodeList),
                SendNodeName = "node_"++integer_to_list(SendNodeNumber),
                {ok, SendNode} = dict:find(SendNodeName, NodeList),
                SendNode ! get_gossip,
                io:format("Node: ~w. Forwarding to ~w.~n",[NodeNumber, SendNodeNumber]),
                full_gossip(NodeNumber, Count, NodeList)
        end
    end.

wait_gossip() ->
    ok.

start_gossip(NumberOfNodes, Topology, Algorithm) ->
    % {ok, NumNodes} = io:read("Enter number of nodes: "),
    % {ok, Topology} = io:read("Enter topology (full | grid | line | imperfect): "),
    % {ok, Algorithm} = io:read("Enter algorithm (gossip | pushsum):"),
    TopologyIsSquare = lists:member(Topology, [grid, imperfect]),
    TopologyIsLinear = lists:member(Topology, [full, line]),
    if
        TopologyIsSquare ->
            Sqrt = round(math:ceil(math:sqrt(NumberOfNodes))),
            NodeList = build_topology(1, 1, Sqrt, dict:new(), Topology, Algorithm),
            register(main_node_list, spawn(gsp, main_node_list, [get_node_list(1, 1, Sqrt, [])])),
            SeedNodeI = rand:uniform(Sqrt),
            SeedNodeJ = rand:uniform(Sqrt),
            SeedNodeName = "node_"++integer_to_list(SeedNodeI)++"_"++integer_to_list(SeedNodeJ),
            io:format("Seed Node: ~p~n", [SeedNodeName]),
            {ok, SeedNode} = dict:find(SeedNodeName, NodeList),
            if
                Algorithm == gossip ->
                    SeedNode ! get_gossip;
                Algorithm == pushsum ->
                    PushSumS = ((SeedNodeI - 1)*Sqrt) + SeedNodeJ,
                    SeedNode ! {get_gossip, [PushSumS, 1]}
            end;
        TopologyIsLinear ->
            NodeList = build_topology(NumberOfNodes, NumberOfNodes, dict:new(), Topology, Algorithm),
            register(main_node_list, spawn(gsp, main_node_list, [lists:seq(1, NumberOfNodes)])),
            SeedNodeNumber = rand:uniform(NumberOfNodes),
            SeedNodeName = "node_"++integer_to_list(SeedNodeNumber),
            io:format("Seed Node: ~p~n", [SeedNodeName]),
            {ok, SeedNode} = dict:find(SeedNodeName, NodeList),
            if
                Algorithm == gossip ->
                    SeedNode ! get_gossip;
                Algorithm == pushsum ->
                    SeedNode ! {get_gossip, [SeedNodeNumber, 1]}
            end
    end,
    statistics(wall_clock),
    io:format("List of nodes ~p ~w ~w~n", [dict:to_list(NodeList), Topology, Algorithm]).
