$(document).ready(function(){

	var currJobID = null;

	function replaceConsole(text) {
		var console = $("#console");
		console.text(text);
		console.parent().scrollTop(console.scrollHeight);
	}

	function appendConsole(text) {
		var console = $("#console");
		console.append(text);
		console.parent().scrollTop(console.scrollHeight);
	}

	function submit() {
		// Extract and pre-process form data
		var ssid = $("#ssid").val();
		var uniprot = $("#uniprot").val();
		var pdb = $("#pdb").val();
		var mc = $("#mc").val();

		// Flash warnings if mandatory fields are missing
		if (!ssid) {
			hilightMissing($("#ssid"))
			return false;
		}
		if (!uniprot) {
			hilightMissing($("#uniprot"))
			return false;
		}
		if (!pdb) {
			hilightMissing($("#pdb"))
			return false;
		}
		if (!mc) {
			hilightMissing($("#mc"))
			return false;
		}

		var wtseq = ($("#wt").val() !== "") ? $("#wt").val() : null;
		var synMed = !($("#synAuto").checked) ? $("#synMed").val() : null;
		var stopMed = !($("#stopAuto").checked) ? $("#stopMed").val() : null;
		var overrideCache = $("#overrideCache").checked;

		// Asynchronous POST request with form data
		$.post("submit.R",
		{
			scoresetID: ssid,
			uniprot: uniprot,
			pdb: pdb,
			mainChain: mc,
			WT: wtseq,
			seqOffset: $("#seqOffset").val(),
			synMed: synMed,
			stopMed: stopMed,
			pngRes: $("#pngRes").val(),
			overrideCache: overrideCache,
		})
		.done(function(rawdata) {
			data = JSON.parse(rawdata)
			currJobID = data.jobID;
			appendConsole(
				"\nSubmitted job " + currJobID +
				"\nWaiting for server response..."
			)
			setTimeout(pollStatus,1000)
		})
		.fail(function(xhr, status, error) {
			alert(error);
		});
	}

	function pollStatus() {
		$.post("status.R",
		{
			jobID: currJobID
		})
		.done(function(rawdata) {
			data = JSON.parse(rawdata);
			replaceConsole(data.log)
			switch(data.status) {
				case "Done":
					showResult();
					break;
				case "Error":
					alert(data.message)
					break;
				case "Processing":
					setTimeout(pollStatus,1000)
					break;
				default:

			}
			if (data.status === "Done") {
				//if results are ready, show them
				showResult()
			} else if (data.match(/Error/)) {
				//if results are ready, show them
				alert("The job encountered an error!")
			} else {
				//otherwise call poll status again after 1000 ms
				setTimeout(pollStatus,1000)
			}
		})
		.fail(function(xhr, status, error) {
			alert(error);
		});
	}

	function showResult() {
		$.post("fetch.R",
		{
			jobID: currJobID,
			format: "png",
			output: "url"
		})
		.done(function(url) {
			$("#imagepanel").html('<img src="'+url+'" alt="result"/>')
		})
		.fail(function(xhr, status, error) {
			alert(error);
		});
	}

	//add a highlight the field with a red background for 5sec
	function hilightMissing(element) {
		appendConsole(
			"\n" + element.prop("name") +
			" is a required input!"
		)
		element.addClass("highlight")
		setTimeout(function() {
			element.removeClass("highlight")
		},5000)
	}

	//Submit button action
	$("#submit").click(submit)

	$("#synAuto").change(function() {
		$("#synMed").prop("disabled",this.checked);
	});

	$("#stopAuto").change(function() {
		$("#stopMed").prop("disabled",this.checked);
	});

});