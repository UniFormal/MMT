var kernel = null;

/*
  message:
    header
    parent (= request) header
    metadata
    content
  for example, metadata may update syntax highlighting information
*/

$(function(){
  var options = {
    baseUrl: "/",
    name: "mmt"
  };
  startNewKernel(options).then(k => {kernel = k})
})

// restart kernel
function restart() {
   kernel.restart()
}

// execute a command and handle replies
function runCommand() {
   var command = $('#shell').get()
   var future = kernel.execute({ code: command } );
   // called once per IOPub message send by the kernel - use this for displaying results
   future.onIOPub = (msg) => {
     $("#output").set(msg.content);
   };
   // called when execution done, no more output (i.e., IOPub message) coming
   future.onDone = () => {
     console.log('Future is fulfilled');
   };
};

// interrupt the kernel (sigint, which kernels should interpret as canceling the current execution)
function interrupt() {
   kernel.interrupt()
}

// wait for future versions of jupyter lab js library
// that provides (but is currently still a bit unstable) a cell object API, which handles code mirror and display of inspect and completion results

// kernel function for getting a tooltip about the current shell content (we must display the tooltip ourselves so far)
function inspect() {
   var request: KernelMessage.IInspectRequest = {
     code: 'hello', cursor_pos: 4, detail_level: 0
   };
   kernel.inspect(request).then(reply => {
     console.log(reply.content.data);
   });
}

// kernel function for auto-completion (we must display the tooltip ourselves so far)
function autocomplete() {
   kernel.complete({ code: 'impor', cursor_pos: 4 } ).then((reply) => {
     console.log(reply.content.matches);
   });
}

