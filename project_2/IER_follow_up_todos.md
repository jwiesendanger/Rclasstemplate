# IER Follow-Up To-Dos

Follow-up work from **ICT/LIS 661 Project 2** (`project_2/ICT_LIS_661_Project_2_Wiesendanger.Rmd` / `.html`).

**About this sample.** The data is a de-identified export of Individual Email Results from Salesforce Marketing Cloud, sent between September 15, 2024 and September 20, 2026, with 99% of sends in the final week (September 14–September 20, 2026). It covers **116 unique emails** from 50 University of Kentucky senders (offices such as the Graduate School, Financial Aid, Admissions and student-success programs), totaling **384,024 sends**, one row per recipient. Recipients fall into these broad groups by email domain: UK (uky.edu) (161,010), Gmail (121,155), Other (39,826), Apple iCloud (30,237), K-12 school (21,396), Yahoo / AOL (5,495), Microsoft (Outlook/Hotmail) (4,905).

## What the data suggests each provider is doing

"Clickers w/ no open" is the key clue: a person has to open an email to click it, so a click with no recorded open means the tracking image never loaded or a machine did the clicking.

| Provider | Recipients | Open rate | Click rate | Click-to-open | Clickers w/ no open | Bounce rate | Hard bounce | Heavy clickers (20+) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| UK (uky.edu) | 161,010 | 30.6% | 2.0% | 6.4% | 20.3% | 0.04% | 0.04% | 5 |
| Gmail | 121,155 | 2.5% | 0.5% | 21.2% | 1.7% | 0.69% | 0.10% | 14 |
| Other | 39,826 | 12.3% | 4.5% | 36.4% | 77.7% | 5.6% | 3.4% | 64 |
| Apple iCloud | 30,237 | 61.5% | 0.5% | 0.8% | 5.8% | 4.1% | 0.21% | 0 |
| K-12 school | 21,396 | 9.6% | 1.5% | 15.3% | 73.3% | 12.3% | 8.8% | 4 |
| Yahoo / AOL | 5,495 | 36.6% | 0.9% | 2.4% | 2.1% | 1.3% | 1.2% | 1 |
| Microsoft (Outlook/Hotmail) | 4,905 | 39.8% | 0.6% | 1.5% | 0.0% | 0.37% | 0.06% | 1 |

| Provider | Opens are probably… | Clicks are probably… | Main issue |
|---|---|---|---|
| UK (uky.edu) | somewhat undercounted | reliable | images off / link scanning for some readers |
| Gmail | low but real | low but real | inbox placement (Promotions/spam) / engagement |
| Apple iCloud | heavily inflated | reliable | Apple Mail Privacy Protection |
| Microsoft, Yahoo / AOL | possibly inflated | mostly reliable | small groups; some automatic opens |
| K-12 school | undercounted | partly automated | bounces and school filters |
| Other | undercounted | heavily inflated | security scanners clicking links |

**Caution:** domain is also a stand-in for audience. UK addresses are mostly current students, faculty and staff; Gmail, iCloud and K-12 addresses are more often prospective students and families. This is mostly one week of sends, so these are patterns to investigate, not proven causes.

**Bottom line:** neither the overall open rate (21.3%) nor the overall click rate (1.6%) means what it appears to. The most trustworthy measure in this sample is click rate for UK, Gmail, Apple, Microsoft and Yahoo recipients.

---

## To do 1: Get to the bottom of what each provider is doing with our sends

**Goal:** for each major provider, know whether our opens and clicks are real human activity, machine activity, or missing.

- [ ] **Apple iCloud (61.5% opens, 0.5% clicks).** Check how much of this is Apple Mail Privacy Protection, which loads email images (including the open-tracking pixel) on Apple's servers whether or not the person reads the email. Opens that are high while clicks match other providers fit this pattern.
- [ ] **Gmail (2.5% opens, 0.5% clicks).** Only 1.7% of Gmail clickers lack an open, so tracking appears to work when Gmail users engage. Focus on why engagement is low:
  - [ ] Are sends landing in the **Promotions** tab or spam? Check deliverability and inbox placement for Gmail specifically (e.g., Google Postmaster Tools, seed tests).
  - [ ] How much is the **audience** (prospective students using personal addresses) rather than the provider? Compare the same email's Gmail vs. UK results.
  - [ ] Still rule out **clipping**: Gmail cuts off long messages ("[Message clipped]"), and a pixel below the cut records no open. Check the size of our largest templates.
- [ ] **"Other" (4.5% clicks, the highest of any group; 78% of clickers have no open; 64 of the 89 heavy clickers).** Check for **automated link scanning**: security gateways and filters at organizations often "click" every link to check it for safety. One recipient row has **1,202 clicks**, which is not human.
  - [ ] List the top domains inside "Other" and their click rates.
  - [ ] Look at recipients with very high click counts or clicks on every link.
- [ ] **UK (uky.edu).** UK mail runs through Microsoft 365. Check whether Microsoft's link and attachment scanning (Safe Links / Defender) is creating opens or clicks before people see the email.
- [ ] **K-12 school domains (9.6% opens, 12.3% bounces, 8.8% hard bounces).** 73% of clickers have no recorded open, so check whether school filters are blocking images and scanning links, and clean up hard-bounced K-12 addresses.
- [ ] **Bounces by provider.** Break bounces down by domain group to see if any provider is rejecting our mail at higher rates.
- [ ] Ask the Marketing Cloud admin / deliverability team whether Marketing Cloud already flags machine opens or bot clicks, and if so, whether that flag can be added to the IER export.

---

## To do 2: Review, then act on these suggestions

**Review first:** go over the Project 2 results (`project_2/ICT_LIS_661_Project_2_Wiesendanger.html`) and the table above with the team, and agree on which numbers we trust.

Suggestions:

1. **Make click rate the main success measure across providers** until we understand opens better. It was the more consistent of the two (0.5%–2.0% for the major providers), while open rates ranged from 2.5% to 61.5%.
2. **Report results by domain group, not just overall.** An overall 21.3% open rate mixes Apple's inflated opens with Gmail's missing ones.
3. **Add fields to the IER export** that would let us separate machine activity from people:
   - date/time of open and of click (machine activity often happens within seconds of the send),
   - device / mail client and user agent, if available,
   - any bot or machine-open flag the platform provides.
4. **Filter out likely bot clicks** before reporting (for example, a click on every link within seconds of delivery, or extreme click counts like the 1,202 row).
5. **Check Gmail template size and pixel placement** so opens aren't lost to clipping.
6. **Use only emails with 100+ recipients** when comparing email performance; small and test sends produce 0% or 100% rates that distort averages.
7. **Don't read too much into weekday patterns yet.** Nearly all volume falls in one week, so each weekday is really one day, and Tuesday's low rate comes mostly from one very large send. Collect several weeks before comparing days.
8. **Re-run the analysis on a longer export** (several weeks or a full term) using the same scripts (`prep_project2_data.R`, the Project 2 `.Rmd`), so trends aren't driven by a single send.

---

## To do 3: Understand what an "open" is and isn't

**Goal:** a shared, plain-language definition the team uses when reporting.

**What an open is:** an email platform records an "open" when a tiny invisible image (a tracking pixel) in the email is downloaded. That's all it measures: the image was loaded.

**What an open is not:**

- **Not proof a person read the email.** Apple Mail Privacy Protection and some security scanners load the pixel automatically.
- **Not a full count of readers.** If images are blocked, the email is clipped, or someone reads the plain-text version, a real reader may never register an open.
- **Not comparable across providers** without adjusting for how each one handles images (see To do 1).

Things to confirm for our setup:

- [ ] Is our IER `Opened` field **unique** (opened at least once) or a count of all opens? (In this export it is 0/1.)
- [ ] Does Marketing Cloud count a **click as an "implied open"** when no pixel loaded? If yes, some opens are really clicks.
- [ ] Are **forwarded** emails counted against the original recipient?
- [ ] Write a one-paragraph team definition of "open," "click" and "click-to-open rate," and add it to our reporting notes.

---

*Created October 2026 from the ICT/LIS 661 Project 2 analysis. Source data stays de-identified; the raw export (`activity_data/sample_data.csv`) is not stored in GitHub.*
