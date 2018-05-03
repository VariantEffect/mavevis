$(document).ready(function(){

	/**
	 * variables to store the parameters for submission.
	 */
	var ssid, uniprot, pdbIDs, pdbMainChains, synMed, stopMed, wt, offset;

	var currJobID;

	////////////////////
	// Error handling //
	////////////////////
	/**
	 * Set up the error dialog box
	 */
	$("#errordialog").dialog({
		autoOpen: false,
		buttons: {
			Close: function() {
				$(this).dialog("close");
			}
		}
	}).parent().addClass("ui-state-error");;

	/**
	 * Shows the error dialog with the given message.
	 */
	function showError(text) {
		$("#errormessage").text(text);
		$("#errordialog").dialog("open");
	}



	/**
	 * Appends text to the console and scrolls to the bottom.
	 */
	function appendConsole(text) {
		var console = $("#console");
		console.append(text);
		console.parent().scrollTop(console.scrollHeight);
	}

	$("#consolepanel").hide();


	////////////////
	// Submission //
	////////////////


	/**
	 * checks if the 
	 */
	function checkIfReady() {
		//are ssid and uniprot fields set?
		if (ssid && uniprot) {
			//does the uniprot entry match the wt sequence?

			//are the pdb fields set?
			if (pdbIDs && pdbMainChains) {

				//enable the submit button
				$("#submitButton").prop('disabled', false);

			}
		}
	}

	/**
	 * Resets the form back to its initial state
	 */ 
	function reset() {

		$("#molecule").val("");
		ssid=null;

		resetMost();
	}

	/**
	 * Resets most of the form back to its initial state, except for the score set
	 */ 
	function resetMost() {

		$("#submitButton").prop('disabled', true);
		$("#outputpanel").hide();

		$("#uniprot").val("");
		uniprot=null;
		$("#uniprotOptions").hide();

		$("#pdb").val("");
		pdbIDs=null;
		pdbMainChains=null;
		$("#pdbOptions").hide();

		$("#synMed").val(1);
		$("#stopMed").val(0);
		synMed=null;
		stopMed=null;
		$("#synOptions").hide();
		$("#stopOptions").hide();

		wt=null;
		offset=0;
	}

	//reset is essentially also the same as initializing everything.
	reset();

	$("#resetButton").click(reset);

	$("#submitButton").click(submit);


	/**
	 * Submit the information currently entered in the form to the
	 * submit.R service via POST and then call the pollStatus() function.
	 */
	function submit() {

		//Express these values as R-compatible strings
		var wtR = wt ? wt : "NULL";
		var synR = synMed ? synMed : "NULL";
		var stopR = stopMed ? stopMed : "NULL";
		var pdbS = pdbIDs.toString();
		var mcS = pdbMainChains.toString();


		appendConsole(
			"scoresetID = "+ssid+
			"\nuniprot = " + uniprot+
			"\npdb = " + pdbS+
			"\nmainChain = " + mcS+
			"\nWT = " + wtR+
			"\nseqOffset = " + offset+
			"\nsynMed = " + synR+
			"\nstopMed = " + stopR+
			"\npngRes = " + 80+
			"\noverrideCache = " + "FALSE"
		);

		$.post("submit.R",
		{
			scoresetID: ssid,
			uniprot: uniprot,
			pdb: pdbS,
			mainChain: mcS,
			WT: wtR,
			seqOffset: offset,
			synMed: synR,
			stopMed: stopR,
			pngRes: 80
		})
		.done(function(rawdata) {
			data = JSON.parse(rawdata);
			// setJobID(data.jobID);
			currJobID = data.jobID;
			// appendConsole(
			// 	"\nSubmitted job " + currJobID +
			// 	"\nWaiting for server response..."
			// )

			$("#imagepanel").hide();
			$("#downloadpanel").hide();
			$("#mainProgressbar").progressbar({
				value: false
			});
			$("#mainProgressbar .progress-label").text("Submitting...");
			$("#mainProgressbar").show();
			$("#outputpanel").show();

			setTimeout(pollStatus,1000);
		})
		.fail(function(xhr, status, error) {
			// alert(error);
			$("#outputpanel").hide();
			showError(error);
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
			loglines = data.log.split("\n");
			latest = loglines[loglines.length - 1];
			// replaceConsole(data.log)
			$("#mainProgressbar .progress-label").text(latest);
			switch(data.status) {
				case "Done":
					showResult();
					break;
				case "Error":
					// alert(data.message)
					$("#outputpanel").hide();
					showError(data.message,"Error");
					break;
				case "Processing":
					setTimeout(pollStatus,1000)
					break;
				default:
					// alert("status service returned an unexpected result!")
					showError("Status monitor service returned an unexpected result!");
			}
		})
		.fail(function(xhr, status, error) {
			// alert(error);
			$("#outputpanel").hide();
			showError(error);
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
			showError(error);
		});

		//set the download target
		$("#jobIDHolder").val(currJobID);


		$("#imagepanel").show();
		$("#downloadpanel").show();
		$("#mainProgressbar").hide();
		//should already be visible, but just in case...
		$("#outputpanel").show();

	}


	////////////////////////
	// Scoreset selection //
	////////////////////////

	/**
	 * Set up autocomplete box for the search field
	 */
	$("#molecule").autocomplete({
		source: "searchScoresets.R",
		minLength: 2,
		delay: 500,
		select: function(event, ui) {

			resetMost();

			ssid = ui.item.ssid;
			//if a uniprot ID is available, automatically fill it in
			if (ui.item.uniprot) {
				$("#uniprot").val(ui.item.uniprot).trigger("change");
			}
			//either way show the uniprot field, so the user can change it
			$("#uniprotOptions").show();
			//if the synonymous scores need to be manually defined, show the required field
			if (ui.item.syn == "manual") {
				$("#synOptions").show();
				$("#synMed").change(function() {
					synMed = $(this).val();
				}).trigger("change");
			}
			//if the stop scores need to be manually defined, show the required field
			if (ui.item.stop == "manual") {
				$("#stopOptions").show();
				$("#stopMed").change(function() {
					stopMed = $(this).val();
				}).trigger("change");
			}
			//store wt sequence and offset
			wt = ui.item.wt;
			offset = ui.item.offset;
		}
	});


	/////////////////////
	// Uniprot Options //
	/////////////////////


	var uniprotRegex = new RegExp(
		"[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}"
	);

	$("#uniprot").change(function() {
		//if it's a valid ID
		if (uniprotRegex.test($("#uniprot").val())) {
			uniprot = $("#uniprot").val();
			//1. check whether the sequence matches the scoreset
			//1.a. if not require user to define offset
			//2. enable PDB selection
			$("#pdbOptions").show();
			//3. begin PDB search in background
			findPdbStructures();
		}
	});

	function checkSequence() {

	}

	/////////////////
	// PDB Options //
	/////////////////

	/**
	 * This stores the JSON data used to populate the PDB table.
	 */
	var pdbData;

	/**
	 * Helper function to add a row to the pdb table
	 * @param tbody the table body object
	 * @param row JSON data holding the values for the row
	 * @param header boolean value determining whether this is a header row or not.
	 */
	function addRow(tbody,row,header) {
		var tag = header ? "th" : "td";
		var rowContent = "<tr>";
		if (!header) {
			rowContent += '<td><input type="checkbox"/></td>';
		}
		$.each(row, function(j,entry) {
			rowContent += "<"+tag+">"+entry+"</"+tag+">";
		});
		rowContent += "</tr>";
		tbody.append(rowContent);
	}

	/**
	 * The title row for the pdb table
	 */ 
	var titles = [
		"Select","PDB","Method","Resolution",
		"Main chains","Start","End","Interactors"
	];

	/**
	 * Calls remote service to find PDB structures 
	 */
	function findPdbStructures() {
		//show the progressbar in the dialog
		$("#pdbProgressbar").progressbar({
			value: false
		});
		$("#pdbProgressbar").show();
		//initially hide the results table
		$("#pdbtable").hide();
		// make POST request
		$.post("findPDBs.R",{
			uniprot: uniprot
		})
		.done(function(data) {
			pdbData = data;
			//clear any existing data from the table
			var tbody = $("#pdbtable").find("tbody");
			tbody.empty();
			//poplate table with data
			addRow(tbody,titles,true);
			$.each(data, function(i,row) {
				addRow(tbody,row,false);
			});
			//hook up the table with click triggers
			// $("#pdbtable tbody tr").each(function(i,tr){
			// 	$(tr).click(function(){
			// 		checkbox = $(tr).find("input:checkbox:first");
			// 		checkbox.prop("checked",!checkbox.is(":checked"));
			// 	})
			// });
			//show the table
			$("#pdbProgressbar").hide();
			$("#pdbtable").show();
		})
		.fail(function(xhr, status, error) {
			showError(error,"Unable to find PDB data: "+error);
		});
	}

	/**
	 * gather the user selection from the pdb table
	 */
	function gatherSelected() {
		pdbIDs = [];
		pdbMainChains = [];
		var descr = "";
		$("#pdbtable tbody tr").each(function(i,tr) {
			checkbox = $(tr).find("input:checkbox:first");
			if ($(checkbox).is(":checked")) {
				var id = pdbData[i-1].pdb;
				var mc = pdbData[i-1].mainChains.charAt(0);
				pdbIDs.push(id);
				pdbMainChains.push(mc);
				descr += id+"#"+mc+" ";
			}
		});
		$("#pdb").val(descr).trigger("change");
	}

	//clicking the pdb button opens a dialog
	$("#pdbButton").click(function(){
		//open the dialog
		$("#pdbDialog").dialog("open");
	});

	//defining the pdb dialog
	$("#pdbDialog").dialog({
		autoOpen: false,
		modal: true,
		width: 800,
		height: 600,
		buttons: {
			OK: function() {
				gatherSelected();
				checkIfReady();
				$(this).dialog("close");
			},
			Cancel: function() {
				$(this).dialog("close");
			}
		}
	});

});