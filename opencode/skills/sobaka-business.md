---
name: sobaka-business
description: Sobaka eenmanszaak business context: Dutch freelancer setup, VAT (BTW) obligations, inkomstenbelasting, WBSO/S&O R&D tax credits, invoicing Prosus AI, e-bike asset depreciation, BV considerations. Use when working on invoices, tax calculations, business administration, or Dutch fiscal questions.
license: private
compatibility: opencode
---

## Business Entity
- **Name**: Sobaka (eenmanszaak)
- **Domain**: sobaka.dev
- **Owner**: Vsevolod (Seva) Prudius
- **Location**: Amsterdam, Netherlands
- **Client**: Prosus AI Research Lab (primary)

## Dutch Tax Obligations

### BTW (VAT)
- Standard rate: 21%
- Quarterly BTW returns (aangifte)
- Invoice format: must include BTW number, KVK number, BTW amount
- B2B within EU: reverse charge (0% + verleggingsregel)

### Inkomstenbelasting
- Annual aangifte (income tax return)
- Zelfstandigenaftrek: entrepreneur deduction (~€5000/yr, hours criterion: 1225h)
- Startersaftrek: first 3 years additional deduction
- MKB-winstvrijstelling: 13.31% profit exemption

### Asset Depreciation
- E-bike (Cannondale Mavaro): business asset, depreciate over useful life
- Minimum 20%/year, max 5 years
- Mixed use (business/private): log business km

## WBSO / S&O (R&D Tax Credits)
- Applies to: OmniPlex robotics research, vLLM benchmarking, Jetson inference R&D
- Benefit: reduction in wage tax / social contributions (for eenmanszaak: fictitious salary reduction)
- Application: submit via RVO.nl, quarterly or annual
- Track hours meticulously per project/activity
- 2026 planning: apply for OmniPlex + Sobaka.dev inference research

## Invoicing Prosus AI
- Invoice in EUR
- Include: KVK, BTW number, IBAN, invoice number, date, description, rate
- Net 30 payment terms typical
- Keep signed contracts / statements of work

## BV Considerations (future)
- BV becomes tax-efficient above ~€100k annual profit
- DGA (director-major shareholder) salary: minimum €56k/yr (2025)
- Currently: eenmanszaak sufficient, monitor threshold

## Tools
- Boekhouden: Moneybird or similar (Dutch-compliant)
- Mileage/hour tracking for deductions and WBSO
