# .guideBrowserUI renders

    Code
      out()
    Output
      <div class="well empty">
        <div class="well input">
          <div class="row">
            <div class="col-sm-12">
              <h4>
                Select up to 20 gRNAs in the on-targets table to
                visualize. All gRNAs must target the same chromosome.
              </h4>
            </div>
          </div>
          <br/>
          <div class="row">
            <div class="col-sm-3">
              <div class="form-group shiny-input-container" style="width:100%;">
                <label class="control-label" id="mock-session-guideBrowserGene-label" for="mock-session-guideBrowserGene">Gene to view on-targets</label>
                <div>
                  <select id="mock-session-guideBrowserGene" class="form-control"></select>
                  <script type="application/json" data-for="mock-session-guideBrowserGene">{"plugins":["selectize-plugin-a11y"]}</script>
                </div>
              </div>
            </div>
            <div class="col-sm-3">
              <div id="mock-session-guideBrowserWindow" class="form-group shiny-input-radiogroup shiny-input-container" role="radiogroup" aria-labelledby="mock-session-guideBrowserWindow-label">
                <label class="control-label" id="mock-session-guideBrowserWindow-label" for="mock-session-guideBrowserWindow">Viewing range</label>
                <div class="shiny-options-group">
                  <div class="radio">
                    <label>
                      <input type="radio" name="mock-session-guideBrowserWindow" value="full" checked="checked"/>
                      <span>Show full gene</span>
                    </label>
                  </div>
                  <div class="radio">
                    <label>
                      <input type="radio" name="mock-session-guideBrowserWindow" value="zoom"/>
                      <span>Zoom in on gRNAs</span>
                    </label>
                  </div>
                </div>
              </div>
            </div>
            <div class="col-sm-3">
              <div id="mock-session-guideBrowserStacking" class="form-group shiny-input-radiogroup shiny-input-container" role="radiogroup" aria-labelledby="mock-session-guideBrowserStacking-label">
                <label class="control-label" id="mock-session-guideBrowserStacking-label" for="mock-session-guideBrowserStacking">On-targets track display</label>
                <div class="shiny-options-group">
                  <div class="radio">
                    <label>
                      <input type="radio" name="mock-session-guideBrowserStacking" value="full" checked="checked"/>
                      <span>Each gRNA on separate line (full)</span>
                    </label>
                  </div>
                  <div class="radio">
                    <label>
                      <input type="radio" name="mock-session-guideBrowserStacking" value="squish"/>
                      <span>gRNAs share line where they fit (squish)</span>
                    </label>
                  </div>
                  <div class="radio">
                    <label>
                      <input type="radio" name="mock-session-guideBrowserStacking" value="dense"/>
                      <span>All gRNAs overlap on single line (dense)</span>
                    </label>
                  </div>
                </div>
              </div>
            </div>
            <div class="col-sm-3" align="center">
              <button id="mock-session-guideBrowserButton" style="width:75%;" type="button" class="btn btn-default action-button">Vizualize on-targets</button>
            </div>
          </div>
        </div>
        <div id="mock-session-guideBrowserPlot" class="shiny-html-output"></div>
      </div>

# .renderBrowserErrorMessage returns correct error message

    Code
      .renderBrowserErrorMessage(list(error = "index"))
    Output
      <br/>
      <br/>
      <div class="row">
        <div class="col-sm-12" align="center">
          <h4 class="error-text">Select gRNAs targeting your chosen gene in the table of on-targets above to view.</h4>
        </div>
      </div>

---

    Code
      .renderBrowserErrorMessage(list(error = "chr"))
    Output
      <br/>
      <br/>
      <div class="row">
        <div class="col-sm-12" align="center">
          <h4 class="error-text">All selected gRNAs must target the same chromosome.</h4>
        </div>
      </div>

---

    Code
      .renderBrowserErrorMessage(list(error = "render"))
    Output
      <br/>
      <br/>
      <div class="row">
        <div class="col-sm-12" align="center">
          <h4 class="error-text">Too many gRNAs to render in the plot. Please include fewer gRNAs in your selection.</h4>
        </div>
      </div>

