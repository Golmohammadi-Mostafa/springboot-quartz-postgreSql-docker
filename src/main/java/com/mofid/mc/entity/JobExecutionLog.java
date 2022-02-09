package com.mofid.mc.entity;

import lombok.*;

import javax.persistence.*;
import java.util.Date;

@EqualsAndHashCode
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@Data
@Entity
@Table(name = "qrtz_job_execution_log")
public class JobExecutionLog {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private Long id;

    @Column(name = "job_name", unique = true, nullable = false)
    private String jobName;

    @Column(name = "start_time")
    @Temporal(TemporalType.TIMESTAMP)
    private Date startTime;

    @Column(name = "next_fire_time")
    @Temporal(TemporalType.TIMESTAMP)
    private Date nextFireTime;

    @Column(name = "last_fire_time")
    @Temporal(TemporalType.TIMESTAMP)
    private Date lastFiredTime;

    @Column(name = "final_fire_time")
    @Temporal(TemporalType.TIMESTAMP)
    private Date finalFireTime;

}
