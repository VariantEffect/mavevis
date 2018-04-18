$(document).ready(function(){

	/** 
	 * The current job ID. Gets set by the submit() method
	 * and read by the pollStatus() and showResult() methods.
	 */
	var currJobID = null;

	/**
	 * Registers the current jobID with all necessary fields
	 */ 
	function setJobID(jobID) {
		currJobID = jobID;
		$("#jobIDHolder").val(jobID);
	}

	/**
	 * Turns the download buttons on or of (depending on job completion)
	 */
	function toggleDownload(enabled) {
		$("#pdfbutton").prop("disabled",!enabled);
		$("#pngbutton").prop("disabled",!enabled);
	}

	/**
	 * Replaces the text in the console and scrolls to the bottom.
	 */
	function replaceConsole(text) {
		var console = $("#console");
		console.text(text);
		console.parent().scrollTop(console.scrollHeight);
	}

	/**
	 * Appends text to the console and scrolls to the bottom.
	 */
	function appendConsole(text) {
		var console = $("#console");
		console.append(text);
		console.parent().scrollTop(console.scrollHeight);
	}

	function displayMessage(text,title) {
		$("#dialog").text(text);
		$("#dialog").dialog("option","title",title)
		$("#dialog").dialog("open");
	}

	/**
	 * Submit the information currently entered in the form to the
	 * submit.R service via POST and then call the pollStatus() function.
	 */
	function submit() {
		// Extract and pre-process form data
		var ssid = $("#ssid").val();
		var uniprot = $("#uniprot").val();
		var pdb = $("#pdb").val();
		var mc = $("#mc").val();

		// Flash warnings if mandatory fields are missing
		if (!ssid) {
			hilightMissing($("#ssid"));
			return false;
		}
		if (!uniprot) {
			hilightMissing($("#uniprot"));
			return false;
		}
		if (!pdb) {
			hilightMissing($("#pdb"));
			return false;
		}
		if (!mc) {
			hilightMissing($("#mc"));
			return false;
		}

		//Express these values as R-compatible strings
		var wtseq = ($("#wt").val() !== "") ? $("#wt").val() : "NULL";
		var synMed = !($("#synAuto").is(':checked')) ? $("#synMed").val() : "NULL";
		var stopMed = !($("#stopAuto").is(':checked')) ? $("#stopMed").val() : "NULL";
		var overrideCache = $("#overrideCache").is(':checked') ? "TRUE" : "FALSE";

		//disable download buttons
		toggleDownload(false);

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
			setJobID(data.jobID);
			appendConsole(
				"\nSubmitted job " + currJobID +
				"\nWaiting for server response..."
			)
			setTimeout(pollStatus,1000)
		})
		.fail(function(xhr, status, error) {
			// alert(error);
			displayMessage(error,"Error");
		});
	}

	/**
	 * Recursively check if results are ready every 1000ms
	 * Once it's ready, stop and call showResult()
	 */
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
					// alert(data.message)
					displayMessage(data.message,"Error");
					break;
				case "Processing":
					setTimeout(pollStatus,1000)
					break;
				default:
					// alert("status service returned an unexpected result!")
					displayMessage("Status monitor service returned an unexpected result!","Error");
			}
		})
		.fail(function(xhr, status, error) {
			// alert(error);
			displayMessage(error,"Error");
		});
	}

	/**
	 * Call the fetch.R webservice to retrieve the result and display
	 * it in the image panel.
	 */
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
			// alert(error);
			displayMessage(error,"Error");
		});

		$("#outputpanel").show("drop");

		//re-enable download buttons
		toggleDownload(true);
	}

	/**
	 * highlight a page element with a red background for 5sec
	 */
	function hilightMissing(element) {
		appendConsole(
			"\n" + element.prop("name") +
			" is a required input!"
		);
		element.addClass("highlight");
		setTimeout(function() {
			element.removeClass("highlight")
		},5000);
	}

	//Hook up the submit button to the submit() function.
	$("#submit").click(submit);

	/* 
	 * disable/enable the synMed/stopMed fields depending on 
	 * the synAuto checkbox status
	 */
	$("#synAuto").change(function() {
		$("#synMed").prop("disabled",this.checked);
	});
	$("#stopAuto").change(function() {
		$("#stopMed").prop("disabled",this.checked);
	});

	$("#dialog").dialog({
		autoOpen: false,
		modal: true,
		buttons: {
			Close: function() {
				$(this).dialog("close");
			}
		}
	});

});