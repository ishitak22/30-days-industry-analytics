# Business Questions

This file defines the core business questions for the Logistics & Supply Chain Performance Analysis project. The goal is to shape a strong Tableau Public dashboard around operational performance, delivery reliability, geographic patterns, customer value, and profitability.

These questions are based on the inspected DataCo supply chain dataset. No dataset changes, Tableau worksheets, calculated fields, or dashboard layout decisions are included here.

---

## 1. Where are delivery delays most concentrated across global markets and regions?

**Business Question**  
Which markets, order regions, countries, and cities experience the highest concentration of late deliveries?

**Why it matters to logistics leaders**  
Delay concentration helps leaders identify geographic bottlenecks, regional carrier issues, customs complexity, or fulfillment network gaps. This supports targeted operational intervention instead of broad, expensive process changes.

**Dataset fields needed**
- `Market`
- `Order Region`
- `Order Country`
- `Order City`
- `Latitude`
- `Longitude`
- `Delivery Status`
- `Late_delivery_risk`
- `Order Id`

**Expected business insight**  
The analysis should reveal whether delays are broadly distributed or clustered in specific regions, markets, or destination cities that need deeper logistics review.

---

## 2. Which shipping modes create the strongest trade-off between speed, reliability, and volume?

**Business Question**  
How do shipping modes compare on real shipping days, scheduled shipping days, late delivery risk, and order volume?

**Why it matters to logistics leaders**  
Shipping mode decisions affect cost, customer experience, and capacity planning. Leaders need to understand whether premium options are actually improving delivery reliability, or whether standard options are carrying volume with acceptable performance.

**Dataset fields needed**
- `Shipping Mode`
- `Days for shipping (real)`
- `Days for shipment (scheduled)`
- `Delivery Status`
- `Late_delivery_risk`
- `Order Id`
- `Sales`
- `Order Profit Per Order`

**Expected business insight**  
The analysis should show which shipping modes are dependable, which modes are overused relative to their performance, and where speed promises may not match actual delivery outcomes.

---

## 3. Which routes or lanes show the biggest gap between scheduled and actual delivery time?

**Business Question**  
Which origin-destination patterns have the largest difference between scheduled shipment days and actual shipping days?

**Why it matters to logistics leaders**  
Delivery time variance is often more actionable than average delivery speed alone. Large gaps can signal unrealistic service promises, route-specific congestion, poor carrier performance, or inefficient fulfillment decisions.

**Dataset fields needed**
- `Order Country`
- `Order State`
- `Order City`
- `Customer Country`
- `Customer State`
- `Customer City`
- `Days for shipping (real)`
- `Days for shipment (scheduled)`
- `Shipping Mode`
- `Delivery Status`
- `Order Id`

**Expected business insight**  
The analysis should identify where the delivery promise is most misaligned with actual performance and where service-level expectations may need adjustment.

---

## 4. Are high-value orders receiving reliable delivery service?

**Business Question**  
Do higher-value orders experience better, worse, or similar delivery performance compared with lower-value orders?

**Why it matters to logistics leaders**  
High-value orders often carry greater customer and margin risk. If these orders are delayed at the same or higher rate than lower-value orders, the business may be under-protecting important revenue.

**Dataset fields needed**
- `Sales`
- `Sales per customer`
- `Order Item Total`
- `Order Profit Per Order`
- `Benefit per order`
- `Delivery Status`
- `Late_delivery_risk`
- `Shipping Mode`
- `Customer Segment`
- `Order Id`

**Expected business insight**  
The analysis should show whether delivery risk is randomly distributed or whether valuable and profitable orders are exposed to avoidable fulfillment issues.

---

## 5. Which product categories drive both commercial value and operational pressure?

**Business Question**  
Which product categories and departments combine high sales or profit with high late delivery risk or shipment volume?

**Why it matters to logistics leaders**  
Some categories may be financially important but operationally difficult to fulfill. Identifying these categories helps leaders prioritize inventory planning, supplier coordination, and fulfillment process improvements.

**Dataset fields needed**
- `Department Name`
- `Category Name`
- `Product Name`
- `Sales`
- `Order Profit Per Order`
- `Order Item Quantity`
- `Delivery Status`
- `Late_delivery_risk`
- `Shipping Mode`
- `Order Id`

**Expected business insight**  
The analysis should separate categories that are commercially strong and operationally healthy from categories that generate value but create delivery strain.

---

## 6. How does delivery performance vary by customer segment?

**Business Question**  
Are consumer, corporate, and home office customers receiving different levels of delivery reliability and fulfillment speed?

**Why it matters to logistics leaders**  
Customer segments may have different service expectations and revenue importance. Segment-level delivery gaps can point to customer experience risk, retention risk, or opportunities for differentiated logistics service.

**Dataset fields needed**
- `Customer Segment`
- `Customer Country`
- `Customer State`
- `Delivery Status`
- `Late_delivery_risk`
- `Days for shipping (real)`
- `Shipping Mode`
- `Sales per customer`
- `Order Profit Per Order`
- `Order Id`

**Expected business insight**  
The analysis should show whether specific customer segments receive consistently better or worse fulfillment outcomes, and whether those outcomes align with commercial value.

---

## 7. Which order statuses represent the largest revenue and fulfillment risk?

**Business Question**  
How much sales, profit, and order volume is tied to risky statuses such as canceled, on hold, suspected fraud, pending payment, or payment review?

**Why it matters to logistics leaders**  
Order status mix affects revenue recognition, customer experience, and operational workload. A high concentration of risky statuses can indicate process friction before fulfillment is even completed.

**Dataset fields needed**
- `Order Status`
- `Type`
- `Sales`
- `Order Profit Per Order`
- `Benefit per order`
- `Delivery Status`
- `Late_delivery_risk`
- `Order Id`
- `order date (DateOrders)`

**Expected business insight**  
The analysis should identify whether revenue risk is stable, seasonal, or concentrated in specific order statuses that require operational or payment process attention.

---

## 8. Are delivery problems seasonal or concentrated during specific periods?

**Business Question**  
How do order volume, late delivery rate, sales, and profit change over time?

**Why it matters to logistics leaders**  
Seasonality can reveal demand surges, capacity constraints, staffing pressure, or network stress. Understanding timing patterns helps leaders plan resources before service quality declines.

**Dataset fields needed**
- `order date (DateOrders)`
- `shipping date (DateOrders)`
- `Order Id`
- `Sales`
- `Order Profit Per Order`
- `Delivery Status`
- `Late_delivery_risk`
- `Days for shipping (real)`
- `Shipping Mode`

**Expected business insight**  
The analysis should reveal whether late deliveries rise during specific months, whether profit follows the same pattern as sales, and whether operational performance weakens during peak periods.

---

## 9. Which combinations of market, shipping mode, and customer segment should leaders prioritize?

**Business Question**  
Which market, shipping mode, and customer segment combinations have the highest operational risk and commercial impact?

**Why it matters to logistics leaders**  
Logistics leaders rarely act on one dimension at a time. Prioritization improves when performance is evaluated across geography, service mode, and customer type together.

**Dataset fields needed**
- `Market`
- `Order Region`
- `Shipping Mode`
- `Customer Segment`
- `Sales`
- `Order Profit Per Order`
- `Delivery Status`
- `Late_delivery_risk`
- `Days for shipping (real)`
- `Order Id`

**Expected business insight**  
The analysis should identify priority segments where operational problems are large enough and commercially important enough to justify management action.

