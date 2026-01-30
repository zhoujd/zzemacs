cuda
====

## URLs

```
## https://docs.nvidia.com/cuda/cuda-programming-guide/04-special-topics/cuda-graphs.html
## https://people.maths.ox.ac.uk/gilesm/cuda/lecs/lec6.pdf
## https://eunomia.dev/others/cupti-tutorial/cuda_graphs_trace/
## https://eunomia.dev/others/cupti-tutorial/cuda_graphs_trace/
## https://www.google.com/search?q=cuda+graph+event
```

## CUDA Events and Graphs

```
## Synchronization:
In general CUDA programming, events are synchronization markers inserted into a
stream to track progress and create dependencies between operations across different streams.

In the context of graphs, these dependencies are built into the static graph structure,
allowing the GPU to manage execution order with hardware acceleration rather than
relying on stream synchronization.

## Graph Nodes:
When creating a CUDA graph (either via stream capture or explicit API calls),
operations involving events are captured as specific node types:

### Record Nodes:
A cudaEventRecord call becomes an event record node.
Each launch of the graph records the associated event, capturing the completion of
all preceding nodes within the graph.

### Wait Nodes:
A cudaStreamWaitEvent call becomes an event wait node.
This node ensures that all operations following it in the graph wait until the
specified event (potentially recorded by a different graph or an external stream) has occurred.

## Timing:
Events with timing enabled (using cudaEventCreate with the cudaEventEnableTiming flag)
can be used to measure the elapsed time between two points in a workflow.
This is done by adding event record nodes at the desired start and end points in the graph,
launching the graph, and then using cudaEventElapsedTime on the host after the graph has completed.

## External Events:
The cudaEventRecordExternal and cudaEventWaitExternal flags are used
when an event needs to manage a dependency between operations inside a graph and
operations in an external stream (or another process),
allowing communication with work not defined within the specific graph capture.

Key Benefits
Using events within CUDA graphs allows for:
## Precise control over operation sequencing and dependencies.
## Accurate performance measurement of specific sections of the graph.
## Efficient synchronization without returning control to the CPU, which drastically reduces CPU overhead.
```
