"""Batch demo: open WPS Writer, insert paragraphs, count."""
app = wps  # bound live WPS Writer application
doc = app.Documents.Add()
doc.Content.Text = "Hello from mcp-wpsapi"
doc.SaveAs(r"C:\temp\mcp_wpsapi_demo.docx")
_ = {"word_count": len(doc.Content.Text.split()), "saved": True}
